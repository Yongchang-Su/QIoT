#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::mat RC_C(arma::mat coeflistS, double U_pre, double alpha, double z){
  int m = coeflistS.n_cols;
  arma::vec U(m, arma::fill::zeros);
  if(m >= 1){
    U = U_pre + trans(coeflistS.row(1)) - alpha*trans(coeflistS.row(0));
  }
  arma::mat lst(2, 0, arma::fill::zeros);
  for(int i=0; i<m; i++){
    if(U[i] > z){
      lst = arma::join_horiz(lst, coeflistS.col(i));
    }
  }
  return lst;
}


// [[Rcpp::export]]

Rcpp::List RS_C(Rcpp::NumericMatrix lst, Rcpp::IntegerVector C, Rcpp::NumericVector Aplus, Rcpp::NumericVector Aminus, Rcpp::NumericVector values){
  double z = values[0], p = values[1], w1 = values[2], p1 = values[3];
  int m = C.size();
  for(int i=0; i<m; i++){
    Aplus.erase(C[i]-1);
    Aminus.erase(C[i]-1);
  }
  double ap = max(Aplus);
  double am = min(Aminus);
  Rcpp::NumericVector z_cal(0);
  m = lst.ncol();
  for(int i=0; i<m; i++){
    if(lst(0,i)<=p){
      z_cal.push_back(lst(1,i));
    }
  }
  z_cal.push_back(z);
  z = max(z_cal);
  Rcpp::NumericVector index(0);
  double U;
  for(int i=0; i<m; i++){
    double cal = p - w1 - lst(0,i);
    if(cal>=0){
      U = lst(1,i)+p1+cal*ap;
    }else{
      U = lst(1,i)+p1+cal*am;
    }
    if(U>z||(U==z&&lst(0,i)!=p)){
      index.push_back(i);
    }
  }
  m = index.size();
  Rcpp::NumericMatrix lst1(2, m);
  for(int i=0; i<m; i++){
    lst1(Rcpp::_, i) = lst(Rcpp::_, index[i]);
  }
  return Rcpp::List::create(
    Rcpp::_["lst"] = lst1,
    Rcpp::_["z"] = z
  );
}
arma::mat merge_list(arma::mat L, arma::mat R){
  arma::vec zeroinf(2, arma::fill::zeros);
  zeroinf[0] = arma::datum::inf;
  L = arma::join_horiz(L, zeroinf);
  R = arma::join_horiz(R, zeroinf);
  arma::mat LR(2, 1, arma::fill::zeros);
  LR(0,0) = arma::datum::inf;
  arma::vec sm(2, arma::fill::zeros);
  do{
    if(L(0,0)<R(0,0)){
      sm = L.col(0);
      L.shed_col(0);
    }else{
      sm = R.col(0);
      R.shed_col(0);
    }
    if(sm[0] < arma::datum::inf){
      arma::vec lg = LR.col(LR.n_cols - 1);
      if(sm[1]>lg[1]){
        if(sm[0]<=lg[0]){
          LR.shed_col(LR.n_cols - 1);
        }
        LR = arma::join_horiz(LR, sm);
      }
    }

  } while (sm[0] < arma::datum::inf);
  return LR;
}

// [[Rcpp::export]]
arma::mat add_C(arma::mat L, arma::mat R, int p){
  int m = R.n_cols;
  arma::rowvec rv(L.n_cols, arma::fill::ones);
  arma::mat LR = L + R.col(0)*rv;
  LR = LR.submat(arma::uvec {0,1}, arma::find(LR.row(0)<=p));
  for(int i=1; i<m; i++){
    arma::mat LR2 = L + R.col(i)*rv;
    LR2 = LR2.submat(arma::uvec {0,1}, arma::find(LR2.row(0)<=p));
    LR = merge_list(LR, LR2);
    }
  return LR;
}

Rcpp::List LpDomRem_C(Rcpp::List coeflist){
  int n = coeflist.length();
  for(int s=0; s<n; s++){
    int i1=0, i2=1, i3=2;
    arma::mat r = coeflist[s];
    while((unsigned)i3 < r.n_cols){
      if(r(1,i3) == r(1,i2)){
        r.shed_col(i3);
        continue;
      }
      if((r(1,i3)-r(1,i2))/(r(0,i3)-r(0,i2))>=(r(1,i2)-r(1,i1))/(r(0,i2)-r(0,i1))){
        r.shed_col(i2);
        if(i1 != 0){
          --i1;
          --i2;
          --i3;
        }
      }else{
        ++i1;
        ++i2;
        ++i3;
      }
    }
    coeflist[s] = r;
  }
  return coeflist;
} 

arma::vec diff(arma::vec v){
  int n = v.n_elem;
  arma::vec v_diff(n-1, arma::fill::zeros);
  for(int i=0; i<n-1; i++){
    v_diff[i] = v[i+1] - v[i];
  }
  return v_diff;
}

double objcal(Rcpp::List coeflist, Rcpp::List sol){
  int n = sol.length();
  double obj = 0;
  for(int i=0; i<n; i++){
    arma::mat Ci = coeflist(i);
    arma::vec Si = sol(i);
    obj += sum(Ci.row(1)*Si);
  }
  return obj;
}
// [[Rcpp::export]]
double On_median(arma::vec x, int k = -1){
  int n = x.n_elem;
  if(n == 1){
    return x(0);
  }
  arma::vec x1(n, arma::fill::value(x(0)));
  if(all(x == x1)){
    return x(0);
  }
  if(k < 0){
    k = floor((n+1)/2);
  }
  int r = rand()%n;
  double piv = x(r);
  arma::vec les = x.elem(find(x<piv));
  arma::vec eq = x.elem(find(x==piv));
  arma::vec grt = x.elem(find(x>piv));
  int nl = les.n_elem;
  int ne = eq.n_elem;
  if(nl>k){
    return On_median(les, k);
  }else if(nl+ne>=k){
    return piv;
  }else{
    return On_median(grt, k - les.n_elem - eq.n_elem);
  }
}
// [[Rcpp::export]]

Rcpp::List LpGreedy_On_C(Rcpp::List coeflist, double p){
  if(p < 0){
    return Rcpp::List::create(Rcpp::_["obj"] = arma::datum::inf, Rcpp::_["sol"] = "no feasible solution");
  }
  double sm = 0;
  int n = coeflist.length();
  Rcpp::List coeflist1(n);
  for(int s=0; s<n; s++){
    arma::mat Cs = coeflist[s];
    double l = Cs(1,0);
    sm += l;
    arma::rowvec lv(Cs.n_cols, arma::fill::ones);
    Cs.row(1) = l*lv - Cs.row(1);
    coeflist1[s] = Cs;
  }
  coeflist1 = LpDomRem_C(coeflist1);
  arma::vec p_w(0, arma::fill::zeros);
  arma::vec w = p_w, strata = p_w;
  for(int s=0; s<n; s++){
    arma::mat Cs = coeflist1[s]; 
    p_w = arma::join_cols(p_w, trans(diff(Cs.row(1))/diff(Cs.row(0))));
    w = arma::join_cols(w, trans(diff(Cs.row(0))));
    arma::vec fs(Cs.n_cols-1, arma::fill::value(s));
    strata = arma::join_cols(strata, fs);
  }
  
  if(p >= sum(w)){
    Rcpp::List sol(n);
    for(int s=0; s<n; s++){
      arma::mat Cs = coeflist[s];
      arma::vec a(Cs.n_cols, arma::fill::zeros);
      a[Cs.n_cols-1] = 1;
      sol[s] = a;
    }
    double obj = objcal(coeflist, sol);
    return Rcpp::List::create(Rcpp::_["obj"] = obj);
  }
  arma::uvec S(p_w.n_elem, arma::fill::zeros);
  for(unsigned int i=0; i<S.n_elem;++i){
    S[i] = i;
  }
  double obj = 0;
  // arma::vec grp(n, arma::fill::zeros);
  double r;
  arma::uvec H, E, L;
  do{
    arma::vec p_w_S = p_w.elem(S);
    r = On_median(p_w_S);
    H = S.elem(arma::find(p_w_S>r));
    E = S.elem(arma::find(p_w_S==r));
    L = S.elem(arma::find(p_w_S<r));
    if(sum(w.elem(H))>p){
      S = H;
    }else if(sum(w.elem(H))+sum(w.elem(E))<=p){
      S = L;
      p -= sum(w.elem(H))+sum(w.elem(E));
      obj += sum(p_w.elem(H)%w.elem(H))+sum(p_w.elem(E)%w.elem(E));
    //  for(int i=0; i<n; i++){
    //    arma::uvec nH = find(strata.elem(H)==i);
     //   arma::uvec nE = find(strata.elem(E)==i);
     //   grp(i) = grp(i) + nH.n_elem + nE.n_elem;
      //}
    }else{
      // for(int i=0; i<n; i++){
      //   arma::uvec nH = arma::find(strata.elem(H)==i);
      //   grp(i) = grp(i) + nH.n_elem;
      // }
      break;
    }
  }while(1);
  obj += sum(p_w.elem(H)%w.elem(H)) + r*(p-sum(w.elem(H)));
  // arma::vec wE = w.elem(E); 
  // arma::vec cumsumw(E.n_elem, arma::fill::zeros);
  // cumsumw[0] = wE[0];
  // for(unsigned int i=1; i<E.n_elem; i++){
  //   cumsumw[i] = cumsumw[i-1] + wE[i];
  // }
  // arma::uvec Eel = E.elem(arma::find(cumsumw>p-sum(w.elem(H))));
  // int ss = Eel[0];
  // Rcpp::List sol(n);
  // for(int s=0; s<n; s++){
  //   arma::mat Cs = coeflist[s], Cs1 = coeflist1[s];
  //   arma::vec a(Cs.n_cols, arma::fill::zeros);
  //   a(0) = 1;
  //   if(s!=strata[ss]){
  //     a[Cs1(0,grp[s])] = 1;
  //     arma::uvec a1 = find(a==1);
  //     if(a1.n_elem>1){
  //       a[0] = 0;
  //     }
  //   }else{
  //     int ind1 = Cs1(0, grp[s]);
  //     int ind2 = Cs1(0, grp[s]+1);
  //     a[ind2] = (p - sum(w.elem(H)))/(ind2 - ind1);
  //     a[ind1] = 1- a[ind2];
  //   }
  //   sol[s] = a;
  // }
  return Rcpp::List::create(Rcpp::_["obj"] = sm-obj);
}

Rcpp::List IPDomRem_C(Rcpp::List coeflist){
  int n = coeflist.length();
  for(int s=0; s<n; s++){
    arma::mat Cs = coeflist[s];
    arma::vec C = Cs.col(0);
    arma::vec d = diff(trans(Cs.row(1)));
    Cs.shed_col(0);
    arma::mat Cs1 = arma::join_horiz(C, Cs.cols(arma::find(d!=0)));
    coeflist[s] = Cs1;
  }
  return coeflist;
}

// [[Rcpp::export]]

Rcpp::List DP_C(Rcpp::List coeflist, double p){
  if(p < 0){
    return Rcpp::List::create(Rcpp::_["obj"] = arma::datum::inf, Rcpp::_["sol"] = "no feasible solution");
  }
  double sm=0;
  int n = coeflist.length();
  Rcpp::List coeflist1(n);
  for(int s=0; s<n; s++){
    arma::mat Cs = coeflist[s];
    double l = Cs(1,0);
    sm += l;
    arma::rowvec lv(Cs.n_cols, arma::fill::ones);
    Cs.row(1) = l*lv - Cs.row(1);
    coeflist1[s] = Cs;
  }
  coeflist1 = IPDomRem_C(coeflist1);
  arma::mat R(n+1, p+1, arma::fill::zeros);
  arma::mat solpos(n, p, arma::fill::zeros);
  for(int i=n-1; i>=0; i--){
    for(int j=0; j<p; j++){
      arma::mat Cs = coeflist1[i];
      int m = Cs.n_cols;
      arma::vec out;
      if(j+2<=m){
        out.set_size(j+2);
        out.fill(-arma::datum::inf);
        for(int t=0; t<j+2; t++){
          if(Cs(0,t)>=j+2) break;
          out[t] = R(i+1, j+1-Cs(0, t)) + Cs(1, t);
        }
      }else{
        out.set_size(m);
        out.fill(-arma::datum::inf);
        for(int t=0; t<m; t++){
          if(Cs(0,t)>=m) break;
          out[t] = R(i+1, j+1-Cs(0, t)) + Cs(1, t);
        }
      }
      
      R(i, j+1) = max(out);
      arma::uvec pos = find(out == R(i, j+1));
      solpos(i,j) = Cs(0,pos[0]);
    }
  }
  Rcpp::List sol(n);
    double ptr = p;
    for(int s=0; s<n; s++){
      arma::mat Cs = coeflist[s];
      arma::rowvec a(Cs.n_cols, arma::fill::zeros);
      if(ptr>0){
        a(solpos(s, ptr-1)) = 1;
        ptr -= Cs(0, solpos(s, ptr-1));
      }else{
        a[0] = 1;
      }
      sol[s] = a;
    }
  arma::rowvec smv(p+1, arma::fill::value(sm));
  for(int t=0;t<p;t++){
    if(R(0,t)==-arma::datum::inf){
      R(0,t) = R(0,t-1);
    }
  }
  return Rcpp::List::create(Rcpp::_["obj"] = sm - R(0, p),
                            Rcpp::_["sol"] = sol,
                            Rcpp::_["objall"] =smv - R.row(0));
}

bool character_equal( Rcpp::CharacterVector x, Rcpp::CharacterVector y){
  bool r = false;
  for( int i=0; i<x.size(); i++){
    if(x[i] == y[i]){
      continue;
    }else{
      return(r);
    }
  }
  r = true;
  return(r);
}

// [[Rcpp::export]]
arma::vec rank_score(int n, Rcpp::List method_list){
  arma::vec score(n);
  Rcpp::CharacterVector name = method_list["name"]; 
  if(character_equal(name, "Wilcoxon")){
    for(int i=0; i<n; i++){
      score[i] = i+1;
    }
    
  }else if(character_equal(name, "Stephenson")){
    int s = method_list["s"];
    for(int i=0; i<n; i++){
      score[i] = Rf_choose(i, s-1);
    }
  }else{
    std::cout<< "error: invalid input for method.list"<< std::endl;
    return 0;
  }
  return score;
}
class Comparator {
private:
  const Rcpp::NumericVector& ref;
  
  bool is_na(double x) const 
  {
    return Rcpp::traits::is_na<REALSXP>(x);    
  }
  
public:
  Comparator(const Rcpp::NumericVector& ref_)
    : ref(ref_)
  {}
  
  bool operator()(const int ilhs, const int irhs) const
  {
    double lhs = ref[ilhs], rhs = ref[irhs]; 
    if (is_na(lhs)) return false;
    if (is_na(rhs)) return true;
    return lhs < rhs;
  }
};

// [[Rcpp::export]]
Rcpp::NumericVector fir_rank(Rcpp::NumericVector x)
{
  R_xlen_t sz = x.size();
  Rcpp::IntegerVector w = Rcpp::seq(0, sz - 1);
  std::sort(w.begin(), w.end(), Comparator(x));
  
  Rcpp::NumericVector r = Rcpp::no_init_vector(sz);
  for (R_xlen_t n, i = 0; i < sz; i += n) {
    n = 1;
    while (i + n < sz && x[w[i]] == x[w[i + n]]) ++n;
    Rcpp::IntegerVector w_sub = Rcpp::clone(w)[Rcpp::Range(i, i+n-1)];
    w_sub.sort();
    for (R_xlen_t k = 0; k < n; k++) {
      r[w_sub[k]] = i + k;
    }
  }
  
  return r;
}

// [[Rcpp::export]]

double min_stat(arma::vec Z, arma::vec Y, int k, double c, Rcpp::List method_list, Rcpp::Nullable<Rcpp::NumericVector> score = R_NilValue){
  int n = Y.n_elem;
  int m = sum(Z);
  arma::vec score2;
  if(score.isNull()){
    score2 = rank_score(n, method_list);
  }else{
    score2 = Rcpp::as<arma::vec>(wrap(score));
  }
  arma::uvec ind_treat;
  ind_treat = arma::find(Z == 1);
  arma::vec xi(n, arma::fill::value(c));
  if(k < n){
    xi.elem(ind_treat.subvec(m-std::min(m, n-k), m-1)).fill(arma::datum::inf);
  }
  arma::vec yz = Y - Z%xi;
  Rcpp::NumericVector yz_nv = Rcpp::NumericVector(yz.begin(), yz.end());
  Rcpp::NumericVector ch = fir_rank(yz_nv);
  arma::uvec ch_uv = Rcpp::as<arma::uvec>(ch);
  double stat_min = sum(score2.elem(ch_uv.elem(arma::find(Z == 1))));
  return stat_min;
}

// [[Rcpp::export]]
arma::umat combnCpp(int n, int k) {
  
  double n_subsets = Rf_choose(n, k);
  arma::umat out = arma::zeros<arma::umat>(k, n_subsets);
  arma::uvec a = arma::linspace<arma::uvec>(1, k, k);  
  out.col(0) = a;
  int m = 0;
  int h = k;
  arma::uvec j;
  
  for(long long i = 1; i < n_subsets; i++){
    if(m < (n - h)){  
      h = 1;
      m = a(k - 1);
      j = arma::linspace<arma::uvec>(1, 1, 1);
    }
    else{
      m = a(k - h - 1);
      ++h;
      j = arma::linspace<arma::uvec>(1, h, h);
    }
    a.elem(k - h - 1 + j) = m + j;
    out.col(i) = a;
  }
  return(out);
}

// [[Rcpp::export]]

arma::vec null_dist(int n, int m, 
                    Rcpp::Nullable<Rcpp::List> method_list_ = R_NilValue,
                    Rcpp::Nullable<Rcpp::NumericVector> score_ = R_NilValue,
                    Rcpp::Nullable<Rcpp::NumericMatrix> Z_perm_ = R_NilValue,
                    float null_max = 1e5
){
  arma::vec score;
  if(method_list_.isNotNull() && score_.isNotNull()){
    std::cout<< "warnings: method.list is not used since score is already an input"<< std::endl;
  }else if(method_list_.isNull() && score_.isNull()){
    std::cout<< "error: method.list and score can't be null at the same time"<< std::endl;
    return 0;
  }else if(score_.isNull()){
    Rcpp::List method_list(method_list_);
    score = rank_score(n, method_list);
  }else{
    score = Rcpp::as<arma::vec>(wrap(score_));
  }
  arma::vec stat_null;
  if(Z_perm_.isNull()){
    if(std::isfinite(null_max)){
      stat_null.set_size(null_max);
      for(int i=0; i<null_max; i++){
        Rcpp::NumericVector zero_to_n(n);
        for(int j=0; j<n; j++){
          zero_to_n[j] = j;
        }
        Rcpp::NumericVector rand_samp = Rcpp::sample(zero_to_n, m);
        stat_null[i] = sum(score.elem(Rcpp::as<arma::uvec>(wrap(rand_samp))));
      }
    }else{
      arma::umat comb_all = combnCpp(n, m);
      stat_null.set_size(comb_all.n_cols);
      for(unsigned int i=0; i<comb_all.n_cols; i++){
        stat_null[i] = sum(score.elem(comb_all.col(i)-1));;
      }
    }
  }else{
    arma::mat Z_perm = Rcpp::as<arma::mat>(Z_perm_);
    stat_null.set_size(Z_perm.n_cols);
    for(unsigned int i=0; i<Z_perm.n_cols; i++){
      stat_null[i] = sum(score.elem(arma::find(Z_perm.col(i)==1)));;
    }
  }
  
  return stat_null;
}
// [[Rcpp::export]]

Rcpp::List test_stat_matrix_block(arma::vec Z, arma::vec Y,
                                  Rcpp::NumericVector block,
                                  double c,
                                  Rcpp::List method_list_all){
  R_xlen_t sz = block.size();
  Rcpp::List block_assign(sz);
  Rcpp::IntegerVector w = Rcpp::seq(0, sz - 1);
  std::sort(w.begin(), w.end(), Comparator(block));
  
  Rcpp::NumericVector r = Rcpp::no_init_vector(sz);
  int count = 0;
  for (R_xlen_t n, i = 0; i < sz; i += n) {
    n = 1;
    while (i + n < sz && block[w[i]] == block[w[i + n]]) ++n;
    Rcpp::IntegerVector w_sub = Rcpp::clone(w)[Rcpp::Range(i, i+n-1)];
    w_sub.sort();
    block_assign[count] = Rcpp::as<arma::uvec>(w_sub);
    count++;
  }
  Rcpp::List Tlist(count); 
  for(int i=0; i<count; i++){
    arma::uvec bb = block_assign[i];
    arma::vec Zb = Z.elem(bb);
    arma::vec Yb = Y.elem(bb);
    int nb = bb.n_elem;
    arma::mat Ti(2, nb+1);
    Ti.row(0) = arma::linspace<arma::rowvec>(0, nb, nb+1);
    Rcpp::List method_list;
    for(int j=0; j<nb+1; j++){
      if(method_list_all.size()==1){
        method_list = method_list_all[0];
      }else{
        method_list = method_list_all[i];
      }
      Ti(1, j) = min_stat(Zb, Yb, nb-j, c, method_list);
    }
    Tlist[i] = Ti;
  }
  return Tlist;
}

// [[Rcpp::export]]
arma::vec null_dist_block_C(arma::vec Z,
                          Rcpp::NumericVector block,
                          Rcpp::List method_list_all,
                          float null_max = 1e5,
                          Rcpp::Nullable<Rcpp::NumericMatrix> Z_perm_ = R_NilValue)
{
  R_xlen_t sz = block.size();
  Rcpp::List block_assign(sz);
  Rcpp::IntegerVector w = Rcpp::seq(0, sz - 1);
  std::sort(w.begin(), w.end(), Comparator(block));
  
  Rcpp::NumericVector r = Rcpp::no_init_vector(sz);
  int count = 0;
  for (R_xlen_t n, i = 0; i < sz; i += n) {
    n = 1;
    while (i + n < sz && block[w[i]] == block[w[i + n]]) ++n;
    Rcpp::IntegerVector w_sub = Rcpp::clone(w)[Rcpp::Range(i, i+n-1)];
    w_sub.sort();
    block_assign[count] = Rcpp::as<arma::uvec>(w_sub);
    count++;
  }
  arma::vec stat_null(null_max, arma::fill::zeros);
  for(int i=0; i<count; i++){
    arma::uvec bb = block_assign[i];
    int nb = bb.n_elem;
    int mb = sum(Z.elem(bb));
    
    Rcpp::List method_list;
    if(method_list_all.size()==1){
      method_list = method_list_all[0];
    }else{
      method_list = method_list_all[i];
    }
    arma::mat Z_perm_i(nb, null_max); 
    if(Z_perm_.isNull()){
      for(int j=0; j<null_max; j++){
        Rcpp::NumericVector zeroone(nb);
        for(int iter=0; iter<mb; iter++){
          zeroone[iter] = 1;
        }
        Z_perm_i.col(j) = Rcpp::as<arma::vec>(Rcpp::sample(zeroone, nb));
      }
    }else{
      arma::mat Z_perm = Rcpp::as<arma::mat>(Z_perm_);
      Z_perm_i = Z_perm.rows(bb);
    }
    stat_null += null_dist(nb, mb, method_list, R_NilValue, Rcpp::wrap(Z_perm_i));
  }
  return stat_null;
}

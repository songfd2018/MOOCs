#include <R.h>
#include <Rinternals.h>
#include <R_ext/Print.h>
#include <Rmath.h>
#include <math.h>

using namespace std;
/* get the list element named str, or return NULL */
extern "C" {

	SEXP getListElement(SEXP list, const char* str) {
		SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);

		for (int i = 0; i < length(list); i++)
			if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
				elmt = VECTOR_ELT(list, i);
				break;
			}
		return elmt;
	}
	
	// Print the integer vector for debug
	void print_int(int* vec, int length){
		int* temp = vec;
		for(int i = 0; i < length; i ++){
			Rprintf("%d,",*temp);
			temp ++;
		}
		Rprintf("\n");
	}

	// Return the bell number
	int BELL(int n){
		
		int res = 0;
		

		if(n == 0 | n == 1){
			res = 1;
		}else{
			int temp[n + 1];
			temp[0] = 1;
			temp[1] = 1;
			for(int i = 2; i < n + 1; i++){
				temp[i] = 0;
				for(int j = 0; j < i; j++){
					temp[i] = temp[i] + (int)choose(i-1,j) * temp[j];
				}
			}
			res = temp[n];
		}
		return res;
	}

	/*
	//Generate overall combinations to select r elements from a length n vector
	void GEN_COMBINATION(int n, int r, int* C){
		
		//Return an n * (n choose r) matrix C,
		// each column of which is a combination 

		int n_com = (int)choose(n,r);
		int* c = C;
		int n_temp; 
		if( r == 0){
			for(int k = 0; k < n; k++){
				*c = 0;
				c++;
			}
		}else if(r==1){
			for(int j = 0; j < n_com; j++){
				for(int k = 0; k < n; k++){
					if(k == j){
						*c = 1;
					}else{
						*c = 0;
					}
					c++;
				}
			}
		}else if(r==n){
			for(int k = 0; k < n; k ++){
				*c = 1;
				c++;
			}
		}else{
			n_temp = (int)choose(n - 1, r - 1);
			int D_dim = n_temp * (n-1);
			int* D = new int[D_dim];
			GEN_COMBINATION(n-1, r-1, D);
			int* d = D;
			for(int j = 0; j < n_temp; j++){
				*c = 1;
				c ++ ;
				for(int k = 0; k < n - 1; k ++){
					*c = *d;
					c++;
					d++;
				}
			}
			delete [] D;

			n_temp = (int)choose(n - 1, r);
			int E_dim = n_temp * (n-1);
			int* E = new int[E_dim];
			GEN_COMBINATION(n - 1 , r , E);
			int* e = E;
			for(int j = 0; j < n_temp; j++){
				*c = 0;
				c ++ ;
				for(int k = 0; k < n - 1; k ++){
					*c = *e;
					c++;
					e++;
				}
			}
			delete [] E;
 		}
	}

	//Generate overall partitions of a set with length n
	void GEN_PARTITION(int n, int* P){ // n >= 1

		// Return an n * Bell(n) matrix P,
		// each column of which is a partition 

		int *c, *q;
		int n_com, C_dim;
		int B, Q_dim;
		int m;
		if(n == 1){
			*P = 1;
		}else{
			int* p = P;
			// the first column of P is all ones
			for(int k = 0; k < n; k ++ ){
				*p = 1;
				p++;
			}
			// the remaining columns
			for(int i = 1; i < n; i ++){
				// i : the number of positions 
				// not belonging to the group 1
				
				// the combination of elements in the group 1
				n_com = (int)choose(n - 1, n - 1 - i);
				C_dim = (n - 1) * n_com;
				int *C= new int[C_dim];		
				GEN_COMBINATION(n - 1, n - 1 - i, C);

				
				B = BELL(i);
				Q_dim = i * B;
				int *Q= new int[Q_dim];	
				GEN_PARTITION(i, Q);
				
				// i corresponds to (n-1 choose i) * Bell(i) columns

				for(int j = 0; j < n_com; j++){

					q = Q;// q pointer to Q_{1,1}

					// j for column index
					for(int r = 0; r < B; r++){
						// P_{1l} = 1
						*p = 1;
						p++;// p pointer to P_{2l}

						c = &(C[(n-1) * j]);
						// c pointer to C_{1,j}

						for(int k = 1; k < n; k ++){
							// k for row index of P
							if(*c == 1){
								*p = 1;
							}else{
								*p = *q + 1;
								q++;// q pointer to Q_{m,r}
							}
							p++;// p pointer to P_{k+1,l}
							c++;// c pointer to C_{k,j}
						}
					}	
				}
				

				delete [] Q;
				delete [] C;
			}
		}
	}
    */

    //Convert an integer to a combination
    void CONVERT_COMBINATION(int m, int r, int n, int* C){
		
		int* c = C; 
        if(r == 0){
            for(int i = 0; i < n; i ++){
                *c = 0;
				c++;
            }
        }else if(r==1){
            for(int i = 0; i < n; i ++){
                if(i == m){
					*c = 1;
				}else{
					*c = 0;
				}
				c++;
            }   
        }else if(r==n){
            for(int i = 0; i < n; i ++){
                *c = 1;
				c++;
            }
        }else if(m==0){
            for(int i = 0; i < r; i ++){
                *c = 1;
				c++;
            }
            for(int i = r; i < n; i ++){
                *c = 0;
				c++;
            }
        }else{
            
            if(m < (int)choose(n-1, r-1)){
                *c = 1;
				c++;
                int temp = m;
                CONVERT_COMBINATION(temp, r-1, n-1, c);
            }else{
                *c = 0;
				c++;
                int temp = m - (int)choose(n-1, r-1);
                CONVERT_COMBINATION(temp, r, n - 1, c);
            }

        }
	}

    //Convert an integer to a partition
    void CONVERT_PARTITION(int j, int n, int* P){

		int* p = P;
		//Rprintf("j = %d, n = %d.\n",j ,n);

        if(j == 0){
            for(int i = 0; i < n; i ++){
                *p = 1;
				p++;
            }
        }else if(j < n){//here j = 1
			for(int i = 0; i < n; i ++){
				if(i == n - j){
					*p = 2;
				}else{
					*p = 1;
				}
				p ++;
			}
		}else{
            int temp = j;
            int k = 0;// the number of elements not belonging to the first group
            int Bk = BELL(k);
			
            while(temp > Bk * (int)choose(n-1,k) - 1){
                temp = temp - Bk * (int)choose(n-1,k);
                k ++;
                Bk = BELL(k);
            }
            int comb_index = temp / Bk;
            int next_j = temp - comb_index * Bk;
			//Rprintf("comb_index = %d, k = %d, next_j = %d.\n",comb_index, k, next_j);
			
            int* C =  new int[n-1];
            CONVERT_COMBINATION(comb_index, n - 1 - k, n - 1, C);
			//Rprintf("C is:\n");
            //print_int(C,n-1);
			
			
            int* Q = new int[k];
            CONVERT_PARTITION(next_j, k, Q);
			//Rprintf("Q with length %d is:\n",k);
            //print_int(Q,k);
			
            *p = 1;
			p ++;
            int *q = Q;
			int *c = C;
            for(int i = 0; i < n - 1; i ++){
                if(*c == 1){
                    *p = 1;
                }else{				
                    *p = *q + 1;
                    q++;	
                }
				c++;
				p++;
            }
			
			//Rprintf("&Q[0] = %d and Q is:\n",&(Q[0]));
			//print_int(Q,k);
			//Rprintf("To Free Q.\n");
			delete [] Q;
			//Rprintf("Q has been freed.\n");
			
			//Rprintf("&C[0] = %d and C is:\n",&(C[0]));
            //print_int(C,n-1);
			//Rprintf("To Free C.\n");
			delete [] C;
			//Rprintf("C has been freed.\n");
			

        }

		//Rprintf("P is:\n");
        //print_int(P,n);
	}

	/*
	void CONVERT_PARTITION2(int j, int n, int* P){

		
		int* C = new int[n];
		 
		for(int i = 0; i < n; i ++){
                P[i] = 1;
				C[i] = 0;
        }
		C[0] = 1;
		int temp_j = j;// temp index
		int temp_n = n;
		int k, Bk, comb_index, temp_i;
		
		while(temp_j > temp_n - 1){
			temp_j = temp_j - temp_n;
			k = 2;
			Bk = BELL(k);
			while(temp_j > Bk * (int)choose(temp_n-1,k) - 1){
            	temp_j = temp_j - Bk * (int)choose(temp_n-1,k);
            	k ++;
            	Bk = BELL(k);
			}

			comb_index = temp_j / Bk;
            temp_j = temp_j - comb_index * Bk;

			int* temp_C =  new int[temp_n-1];
			//Rprintf("comb_index = %d, temp_n = %d, k = %d\n",comb_index, temp_n, k);

            CONVERT_COMBINATION(comb_index, temp_n - 1 - k, temp_n - 1, temp_C);
			//Rprintf("temp_C is: \n");
			//print_int(temp_C, temp_n - 1);

			temp_i = 0;
			for(int i = 1; i < n; i ++){
				if(C[i] == 0){
					if(temp_C[temp_i] == 1){
						C[i] = 1;
						
					}else{

						if(temp_i==0){
							C[i] = 1;
						}
						P[i] = P[i] + 1;
						
					}
					temp_i ++;
					
				}
			}
			temp_n = k;
			delete [] temp_C;
		}

		if(temp_j > 0){
			temp_i = 0;
			for(int i = 1; i < n; i ++){
				if(C[i] == 0){
					if(temp_i == temp_n - temp_j){
						P[i] = P[i] + 1;
					}
					temp_i ++;
				}
			}
		}
		delete [] C;

	}
	*/

	int max_int(int* P, int n){
		int max = P[0];

		for(int i = 1; i < n; i++){
			if(max < P[i]){
				max = P[i];
			}
		}

		return max;
	}

	void count_int(int* P, int max, int n, int* Count){
		
		int temp;

		for(int l = 0; l < max; l++){
			Count[l] = 0;
		}
		for(int i = 0; i < n; i++){
			temp = P[i] - 1;
			Count[temp] = Count[temp] + 1;
		}

	}

	void STAND_PART(int* T, int n, int* standardized_T){
		
		if( n == 1){
			standardized_T[0] = 1;
		}else{
			int k = T[0];

		//count the number of elements 
		// belonging to the first group
		int num_first = 1;
		for(int i = 1; i < n; i++){
			if(T[i] == k){
				num_first ++;
			}
		}

		if(num_first == n){
			for(int i = 0; i < n; i++){
				standardized_T[i] = 1;
			}
		}else{
			int num_other = n - num_first;
			
			//debug
			//Rprintf("num_first = %d\n", num_first);
			//Rprintf("num_other = %d\n", num_other);

			int* R = new int[num_other];
			int j = 0;
			for(int i = 1; i < n; i++){
				if(T[i] != k){
					R[j] = T[i];
					j ++;
				}
			}
			int* standardized_R = new int[num_other];
		
			STAND_PART(R, num_other, standardized_R);
			delete [] R;

			j = 0;
			for(int i = 0; i < n; i++){
				if(T[i] == k){
					standardized_T[i] = 1;
				}else{
					standardized_T[i] = standardized_R[j] + 1;
					j ++;
				}
			}
			delete [] standardized_R;
			}
		}


	}

	void SUB_PART(int* S, int* A, int n, int* B, int m, int* T){
		
		int i = 0;
		int* R= new int[m];
		for(int j = 0; j < m; j++){
			while(A[i] < B[j]){
				i ++;
			}
			R[j] = S[i];
		}
		
		//debug
		//Rprintf("Finished generating R.\n");
		//Rprintf("R is:.\n");
		//print_int(R, m);
		//Rprintf("T is:.\n");
		//print_int(T, m);

		STAND_PART(R, m, T);

		delete [] R;
	}
	
	void CONVERT_BOOLEAN(int m, int n, int* b){
		int temp = m;
		for(int i = 0; i < n; i++){
			b[n-1-i] = temp - temp / 2 * 2;
			temp = temp / 2;
		}
	}
	
	void MERGE_PART_BOOL(int* A, int l, int* S,  
						int* AB_inter, int m, int* T, 
						int* H, int q,
						int* C, int n, int* R){
		// S for the partition of A with length l
		// AB_inter is the intersection of A and B
		// T for the partition of AB_inter with length m
		// H for the boolean vector of B with length q
		// The length of H is Lt + Ot 
		// C is the union of A and B with length n 
		// Notice that A and C are ordered
		

		// Convert the frist Lt elments H 
		int L_S = max_int(S, l);
		int L_T = max_int(T, m);
		int k = L_S - L_T + q;
		int* Bool_R = new int[k];
		int g;// group index of Bool_R
		for(g = 0; g < k; g++){
			Bool_R[g] = 0;
		}
		int j = 0;
		int i; // the index of A
		int h; // group index of H
		for(int i = 0; i < l; i ++){
			if(A[i] == AB_inter[j]){
				g = S[i] - 1;
				h = T[j] - 1;
				if(H[h] == 1){
					Bool_R[g] = 1;
				} 
				j++;
			}
		}
		
		for(g = L_S; g < k; g++){
			h = g - L_S + L_T;
			Bool_R[g] = H[h];
		}
		
		g = 0;// the first group selected to be connected
		while(Bool_R[g] == 0){
			g ++;
		}
		g ++;

		/*
		int* b = Bool_R;
		Rprintf("Bool_R is: \n");
		for(int ind = 0; ind < k; ind++){
			Rprintf("%d",*b);
			b ++;
		}
		Rprintf("\n");
		*/

		int* R_nostand = new int[n];

		int num_group = L_S; // The number of groups
		int group;

		i = 0;
		for(int j = 0; j < n; j ++){
			if(A[i]==C[j]){
				group = S[i];
				i++;
			}else{
				num_group ++;
				group = num_group;
			}
			if(Bool_R[group - 1] == 1){
				R_nostand[j] = g;
			}else{
				R_nostand[j] = group;
			}
		}

		STAND_PART(R_nostand, n, R);

		delete[] Bool_R;
		delete[] R_nostand;

	}

	int MATCH_COMB(int* C, int n){

		int r = 0;
		for (int i = 0; i < n; i ++){
			r = r + C[i];
		}
		int index = 1;
		int j = 0;
		while(r > 0){
			if(C[j] == 1){
				r = r - 1; 
			}else{
				index = index + (int) choose(n - j - 1, r - 1);
			}
			j ++;
		}
		return index;
	}

	int MATCH_PART(int* P, int n){
		
		int index;
		if(n == 1){
			index = 1;
		}
		else {
			// Count the number of elements not belonging to the first group
			int c = 0;
			// Record the indicators of the first group except the first elements
			int* Comb = new int[n - 1];
			for (int i = 0; i < n - 1; i++) {
				if (P[i + 1] == 1) {
					Comb[i] = 1;
				}
				else {
					Comb[i] = 0;
					c++;
				}
			}

			if (c == 0) {
				index = 0;
			}
			else if (c == 1) {
				index = MATCH_COMB(Comb, n - 1);
			}
			else {

				index = 0;
				// Record the partition of other groups
				int *Q = new int[c];
				int q = 0;
				for (int i = 0; i < n - 1; i++) {
					if (Comb[i] == 0) {
						Q[q] = P[i + 1] - 1;
						q++;
					}
				}

				// debug
				// Rprintf("Q is\n");
				// print_int(Q,c);

				for (q = 0; q < c - 1; q++) {
					index = index + BELL(q + 1) * (int)choose(n - 1, q + 1);
				}
				index = index + BELL(c) * (MATCH_COMB(Comb, n - 1) - 1);
				index = index + MATCH_PART(Q, c) + 1;

				delete [] Q;
			}

			delete [] Comb;
			
		}
		
		
		return index;
	}

	SEXP Cal_prob_Nminus1(SEXP args) {

		int nProtect = 0;
		//Rprintf("Input data.\n");

		// load list from R into Cpp 
		// load the length of CD_inter
		// Here Mt = Nt
		//Rprintf("Load Mt.\n");
		int Mt = INTEGER(getListElement(args, "Mt"))[0];
		

		// load the intersection of C_N and D_N-1
		int* CD_inter = INTEGER(getListElement(args, "CD_inter"));
		

		// load the length of CD_diff
		//Rprintf("Load Ot.\n");
		int Ot = INTEGER(getListElement(args, "Ot"))[0];


		// load the complement of D_N-1 in C_N
		int* CD_diff = INTEGER(getListElement(args, "CD_diff"));

		// load the grading proportion of each student
		//Rprintf("Load lambda.\n");
		double lambda = REAL(getListElement(args, "lambda"))[0];

		//Rprintf("Input has finished.\n");

        int Bt = BELL(Mt);
		SEXP res = PROTECT(allocVector(REALSXP, Bt));
		nProtect ++;
		// Including:
		// Partition: All Bell(N_t) possible partitions
		// prob_Yt: Identifiable probability of all Bell(N_t) circumstances

		// int* part = INTEGER(Partition);
		double* prob = REAL(res);

		//int* part = new int[Bt * Mt];	
		//GEN_PARTITION(Mt, part);

		int* Tj = new int[Mt];
		

  		for(int j = 0; j < Bt; j ++){
			prob[j] = pow(lambda, Ot);
			//Tj = &(part[Mt * j]);
			//Tj pointer to S_{0j}
			CONVERT_PARTITION(j, Mt, Tj);
			//Rprintf("T %d is: \n", j);
			//print_int(Tj, Mt + 2);

			//When t = N - 1, E_t = C_{t+1}
			int Lj;
			Lj = max_int(Tj, Mt);

			if(Lj > 1){
				int* nj = new int[Lj];
				count_int(Tj, Lj, Mt, nj);
				for(int l = 0; l < Lj; l++){
					prob[j] = prob[j] * (1.0-pow(1.0-lambda, nj[l]));
				} 

				delete[] nj;
			}
		}
		
		/*

		// test for BELL function
		Rprintf("The bell number of %d, %d, %d are %d, %d, %d\n",3,4,5,BELL(3),BELL(4),BELL(5));

		// test for GEN_COMBINATION function
		int n = 5;
		int r = 3;
		int n_com = (int)choose(n,r);
		int C_dim = n_com * n;
		int *C= new int[C_dim];// n rows and (n choose r) columns
		
		GEN_COMBINATION(n, r, C);

		int* c= C;
		for(int i = 0; i < n_com; i ++){
			for(int j = 0; j < n; j ++ ){
				Rprintf("%d ",*c);
				c++;
			}
			Rprintf("\n");
		}
		

		delete [] C;
		*/

		delete [] Tj;
		UNPROTECT(nProtect);
		return res;
	}

	SEXP Cal_prob_T(SEXP args) {

		int nProtect = 0;
		//Rprintf("Input data.\n");

		// load list from R into Cpp 
		// load the indicator that D_t cover all submissions
		//Rprintf("Load n_Dt.\n");
		bool COVER_ALL = LOGICAL(getListElement(args, "cover_all_sub"))[0];

		// load the length of ED_inter
		//Rprintf("Load Nt.\n");
		int Nt = INTEGER(getListElement(args, "Nt"))[0];
		
		
		// load the intersection of E_t and D_t
		int* ED_inter = INTEGER(getListElement(args, "ED_inter"));


		// load the length of CD_inter
		//Rprintf("Load Mt.\n");
		int Mt = INTEGER(getListElement(args, "Mt"))[0];
		

		// load the intersection of C_t+1 and D_t
		int* CD_inter = INTEGER(getListElement(args, "CD_inter"));

		// load the length of CD_diff
		//Rprintf("Load Ot.\n");
		int Ot = INTEGER(getListElement(args, "Ot"))[0];


		// load the intersection of E_t and D_t+1
		int* ED_inter2 = INTEGER(getListElement(args, "ED_inter2"));
		
		// load the length of ED_inter3
		int Ntp1 = INTEGER(getListElement(args, "Ntp1"))[0];
		// load the intersection of E_t+1 and D_t+1
		int* ED_inter3 = INTEGER(getListElement(args, "ED_inter3"));

		// load the grading proportion of each student
		//Rprintf("Load lambda.\n");
		double lambda = REAL(getListElement(args, "lambda"))[0];

		// load the probability of circumstances at t + 1
		double* prob_Ytp1 = REAL(getListElement(args, "prob_Ytp1"));


		//Rprintf("Input has finished.\n");
        int Bt = BELL(Nt);
		SEXP res = PROTECT(allocVector(REALSXP, Bt));
		nProtect ++;
		double* prob = REAL(res);

		//int* part = new int[Bt * Nt];
		//Rprintf("Generate the overall partitions.\n");
		//GEN_PARTITION(Nt, part);
		
		int* Sj = new int[Nt];
		int* Tj = new int[Mt];
		
		int* check_Rjq = new int[Ntp1];

		int Lj, num_opt, num_bool, l, n_Rt, rho;
		
		n_Rt = Nt + Ot;
		int* Rjq = new int[n_Rt];

        // debug
        // int ind_print = 1000;

		//Rprintf("Consider Bt = %d differenet circumstances.\n",Bt);
  		for(int j = 0; j < Bt; j ++){
            
            //ind_print = ind_print - 1;
            //if(ind_print == 0){
            //    ind_print = 1000;
            //    Rprintf("Finish the %d-th cirumstance.\n", j);
            //}

			//Sj = &(part[Nt * j]);
			CONVERT_PARTITION(j,Nt,Sj);
			//Sj pointer to the j-th column

			//Rprintf("Obtain the subparitions.\n");
			SUB_PART(Sj, ED_inter, Nt, CD_inter, Mt, Tj);

			//debug
			//Rprintf("S %d is: \n", j);
			//print_int(Sj, Nt);
			//Rprintf("T %d is: \n", j);
			//print_int(Tj, Mt);
			//When t = N - 1, E_t = C_{t+1}
			
			Lj = max_int(Tj, Mt);

			
			int* nj = new int[Lj];
			count_int(Tj, Lj, Mt, nj);

			prob[j] = 0;
			// num_opt : the number of possibly connected groups 
			num_opt = Lj + Ot;
			// num_bool : the number of the equivalent grading circumstances
			num_bool = pow(2,num_opt);

			for(int q = 0; q < num_bool; q++){
				
				int* Hq = new int[num_opt];
				CONVERT_BOOLEAN(q, num_opt, Hq);

				double prob_Hq = 1;
				for(int k = 0; k < Lj; k ++){
					if(Hq[k] == 1){
						// the t-th student grades at least one submission
						// in the k-th groups of Tj
						prob_Hq = prob_Hq * (1 - pow(1-lambda, nj[k]));
					}else{
						// the t-th student doesn't grade any submission
						// in the k-th groups of Tj
						prob_Hq = prob_Hq * pow(1-lambda, nj[k]);
					}
				}

				for(int k = 0; k < Ot; k ++){
					l = k + Lj;
					if(Hq[l] == 1){
						// the t-th student grades the k-th submission
						// of D_t cap C_t+1
						prob_Hq = prob_Hq * lambda;
					}else{
						// the t-th student doesn't grade the k-th submission
						// of D_t cap C_t+1
						prob_Hq = prob_Hq * (1-lambda);
					}
				}

				
				
				MERGE_PART_BOOL(ED_inter, Nt, Sj,// The set E_t cap D_t
								CD_inter, Mt, Tj,// The set C_t+1 cap D_t
								Hq, num_opt, 
								ED_inter2, n_Rt, Rjq);//The set E_t cap D_t+1
				//debug
				//Rprintf("R_%d%d is:\n", j, q);
				//print_int(Rjq, n_Rt);
				
				delete[] Hq;

				if(COVER_ALL & max_int(Rjq, n_Rt) == 1){
					
					prob[j] = prob[j] + prob_Hq;
					
				}else{
					
					SUB_PART(Rjq, ED_inter2, n_Rt, ED_inter3, Ntp1, check_Rjq);

					if(max_int(check_Rjq, Ntp1) == max_int(Rjq, n_Rt)){
						rho = MATCH_PART(check_Rjq, Ntp1);
						
						// debug
						// if(j == 0){
						// 	Rprintf("rho = %d\n",rho);
						//}
						// Rprintf("check_R_%d%d is:\n", j, q);
						// print_int(check_Rjq, Ntp1);
						// Rprintf("rho = %d\n", rho);

						prob[j] = prob[j] + prob_Ytp1[rho] * prob_Hq;
						
						// debug
						// if(j == 0){
						//	Rprintf("prob_1=%f, prob_Ytp1 = %f, prob_Hq = %f\n",prob[j], prob_Ytp1[rho], prob_Hq);
						// }
					}
				}
				
 			
			}//the end of loop for q from 1 to 2^L
			
			delete[] nj;
		}//the end of loop for j from 1 to Bt
		delete[] Sj;
		delete[] Tj;
		delete[] Rjq;
		delete[] check_Rjq;
        //delete[] part;
 		UNPROTECT(nProtect);
		return res;
	}

    SEXP Cal_prob_T_learn(SEXP args) {

		int nProtect = 0;
		//Rprintf("Input data.\n");
		// load list from R into Cpp 
		// load the length of ED_inter
		//Rprintf("Load Nt.\n");
		int Nt = INTEGER(getListElement(args, "Nt"))[0];
		
		// load the intersection of E_t and D_t
		int* ED_inter = INTEGER(getListElement(args, "ED_inter"));

		// load the length of CD_inter
		//Rprintf("Load Mt.\n");
		int Mt = INTEGER(getListElement(args, "Mt"))[0];
		

		// load the intersection of C_t+1 and D_t
		int* CD_inter = INTEGER(getListElement(args, "CD_inter"));

		// load the length of CD_diff
		//Rprintf("Load Ot.\n");
		int Ot = INTEGER(getListElement(args, "Ot"))[0];


		// load the intersection of E_t and D_t+1
		int* ED_inter2 = INTEGER(getListElement(args, "ED_inter2"));
		
		// load the length of ED_inter3
		int Ntp1 = INTEGER(getListElement(args, "Ntp1"))[0];
		// load the intersection of E_t+1 and D_t+1
		int* ED_inter3 = INTEGER(getListElement(args, "ED_inter3"));


		//Rprintf("Input has finished.\n");
        int Bt = BELL(Nt);

        SEXP res;
		
		SEXP linear_comb = PROTECT(allocVector(VECSXP, Bt));
		nProtect++;

		SEXP num_element = PROTECT(allocVector(VECSXP, Bt));
		nProtect++;


		PROTECT(res = allocVector(VECSXP, 2));
		SET_VECTOR_ELT(res, 0 , linear_comb);
		SET_VECTOR_ELT(res, 1, num_element);
		nProtect++;


		//int* part = new int[Bt * Nt];
		
		//Rprintf("Generate the overall partitions.\n");
		//GEN_PARTITION(Nt, part);
		
		int* Sj = new int[Nt];
		int* Tj = new int[Mt];
		int* check_Rjq = new int[Ntp1];
		int Lj, num_opt, num_bool, l, n_Rt, rho;
		n_Rt = Nt + Ot;
		int* Rjq = new int[n_Rt];

		//Rprintf("Consider Bt = %d differenet circumstances.\n",Bt);
  		for(int j = 0; j < Bt; j ++){
			
			//Sj = &(part[Nt * j]);
            CONVERT_PARTITION(j, Nt, Sj);
			//Sj pointer to the j-th column

			//Rprintf("Obtain the subparitions.\n");
			SUB_PART(Sj, ED_inter, Nt, CD_inter, Mt, Tj);	
			Lj = max_int(Tj, Mt);
			
			int* nj = new int[Lj];
			count_int(Tj, Lj, Mt, nj);

			// prob[j] = 0;
			// num_opt : the number of possibly connected groups 
			num_opt = Lj + Ot;
			// num_bool : the number of the equivalent grading circumstances
			num_bool = pow(2,num_opt);

            SEXP LCj = PROTECT(allocVector(INTSXP, num_bool));
            SET_VECTOR_ELT(linear_comb, j , LCj);

            SEXP NEj = PROTECT(allocVector(INTSXP, num_opt));
            SET_VECTOR_ELT(num_element, j , NEj);

            int* lc = INTEGER(LCj);
            int* ne = INTEGER(NEj);

            for(int k = 0; k < Lj; k ++){
                ne[k] = nj[k];
            }
            for(int k = 0; k < Ot; k ++){
                ne[k + Lj] = 1;
            }

			for(int q = 0; q < num_bool; q++){
				
				int* Hq = new int[num_opt];
				CONVERT_BOOLEAN(q, num_opt, Hq);

                /*
				double prob_Hq = 1;
				for(int k = 0; k < Lj; k ++){
					if(Hq[k] == 1){
						// the t-th student grades at least one submission
						// in the k-th groups of Tj
						prob_Hq = prob_Hq * (1 - pow(1-lambda, nj[k]));
					}else{
						// the t-th student doesn't grade any submission
						// in the k-th groups of Tj
						prob_Hq = prob_Hq * pow(1-lambda, nj[k]);
					}
				}
                

				for(int k = 0; k < Ot; k ++){
					l = k + Lj;
					if(Hq[l] == 1){
						// the t-th student grades the k-th submission
						// of D_t cap C_t+1
						prob_Hq = prob_Hq * lambda;
					}else{
						// the t-th student doesn't grade the k-th submission
						// of D_t cap C_t+1
						prob_Hq = prob_Hq * (1-lambda);
					}
				}
                */
				
				
				MERGE_PART_BOOL(ED_inter, Nt, Sj,// The set E_t cap D_t
								CD_inter, Mt, Tj,// The set C_t+1 cap D_t
								Hq, num_opt, 
								ED_inter2, n_Rt, Rjq);//The set E_t cap D_t+1
				//debug
				//Rprintf("R_%d%d is:\n", j, q);
				//print_int(Rjq, n_Rt);
				
				delete[] Hq;

								
				SUB_PART(Rjq, ED_inter2, n_Rt, ED_inter3, Ntp1, check_Rjq);

				if(max_int(check_Rjq, Ntp1) == max_int(Rjq, n_Rt)){
					
                    rho = MATCH_PART(check_Rjq, Ntp1);
					
                    lc[q] = rho;


					// debug
					// if(j == 0){
					// 	Rprintf("rho = %d\n",rho);
					//}
					// Rprintf("check_R_%d%d is:\n", j, q);
					// print_int(check_Rjq, Ntp1);
					// Rprintf("rho = %d\n", rho);

					//prob[j] = prob[j] + prob_Ytp1[rho] * prob_Hq;
						
					// debug
					// if(j == 0){
					//	Rprintf("prob_1=%f, prob_Ytp1 = %f, prob_Hq = %f\n",prob[j], prob_Ytp1[rho], prob_Hq);
					// }
				}else{
                    lc[q] = -1;
                }
				
				
 			
			}//the end of loop for q from 1 to 2^L
			
			delete[] nj;
            UNPROTECT(2); // free the SEXP type variable: linear_comb
		}//the end of loop for j from 1 to Bt
		
		delete[] Tj;
        delete[] Sj;
		delete[] Rjq;
		delete[] check_Rjq;

        //delete[] part;
		UNPROTECT(nProtect);
		return res;
	}

    SEXP Cal_prob_T_linear(SEXP args) {

		int nProtect = 0;
		//Rprintf("Input data.\n");

		// load list from R into Cpp 
        // load the linea
		SEXP linear_comb = getListElement(args, "linear_comb");
		int Bt = length(linear_comb);

        SEXP num_element = getListElement(args, "num_element");

		// load the grading proportion of each student
		//Rprintf("Load lambda.\n");
		double lambda = REAL(getListElement(args, "lambda"))[0];

		// load the probability of circumstances at t + 1
		double* prob_Ytp1 = REAL(getListElement(args, "prob_Ytp1"));


		//Rprintf("Input has finished.\n");
		SEXP res = PROTECT(allocVector(REALSXP, Bt));
        nProtect++;

        double* prob = REAL(res);
		
        for(int j = 0; j < Bt; j++){

            SEXP LCj = VECTOR_ELT(linear_comb, j);
            SEXP NEj = VECTOR_ELT(num_element, j);

            int Qj = length(LCj);
            int num_opt = length(NEj);

            int* lcj = INTEGER(LCj);
            int* nej = INTEGER(NEj);

            prob[j] = 0.0;

            for(int q = 0; q < Qj; q++){
                int* Hq = new int[num_opt];
				CONVERT_BOOLEAN(q, num_opt, Hq);
                
                int rho = lcj[q];
                if(rho > -1){
                    double prob_Hq = 1;
				    for(int k = 0; k < num_opt; k ++){
					    if(Hq[k] == 1){
						// the t-th student grades at least one submission
						// in the k-th groups of Tj
						    prob_Hq = prob_Hq * (1 - pow(1-lambda, nej[k]));
					    }else{
						// the t-th student doesn't grade any submission
						// in the k-th groups of Tj
						    prob_Hq = prob_Hq * pow(1-lambda, nej[k]);
					    }
				    }

                    prob[j] = prob[j] + prob_Ytp1[rho] * prob_Hq;
                }
                

                delete[] Hq;
            }
        }

		UNPROTECT(nProtect);
		return res;
	}

	SEXP Cal_prob_0(SEXP args) {

		int nProtect = 0;
		//Rprintf("Input data.\n");

		// load list from R into Cpp 
		// load the length of C_1
		//Rprintf("Load Ot.\n");
		int Ot = INTEGER(getListElement(args, "Ot"))[0];

		// load C_1
		int* C1 = INTEGER(getListElement(args, "C1"));

		// load the length of ED_inter3
		int Ntp1 = INTEGER(getListElement(args, "Ntp1"))[0];

		// load the intersection of E_t+1 and D_t+1
		int* ED_inter3 = INTEGER(getListElement(args, "ED_inter3"));

		// load the grading proportion of each student
		//Rprintf("Load lambda.\n");
		double lambda = REAL(getListElement(args, "lambda"))[0];

		// load the probability of circumstances at t + 1
		double* prob_Y1 = REAL(getListElement(args, "prob_Y1"));


		//Rprintf("Input has finished.\n");

		// MCMC parameter
		SEXP res = PROTECT(allocVector(REALSXP, 1));
		nProtect++;

		double* prob = REAL(res);

		int rho;  	
		
		*prob = 0;

		// num_bool : the number of the equivalent grading circumstances
		int num_bool = pow(2,Ot);
		int* Hq = new int[Ot];
		int* Rq = new int[Ot];
		int* Rtemp = new int[Ot];
		int* check_Rq = new int[Ntp1];

		for(int q = 0; q < num_bool; q++){
				
			
			CONVERT_BOOLEAN(q, Ot, Hq);

			//Rprintf("H%d is:", q);
			//print_int(Hq, Ot);
			
			double prob_Hq = 1;
			
			int g = 1;
			for(int k = 0; k < Ot; k ++){
				if(Hq[k] == 1){
					// the first student grades the k-th submission
					prob_Hq = prob_Hq * lambda;
					Rtemp[k] = 1;
				}else{
					// the first student doesn't grade the k-th submission
					prob_Hq = prob_Hq * (1-lambda);
					g ++;
					Rtemp[k] = g;
				}
			}
			
			STAND_PART(Rtemp, Ot, Rq);
			
			SUB_PART(Rq, C1, Ot, ED_inter3, Ntp1, check_Rq);

			if(max_int(check_Rq, Ntp1) == max_int(Rq, Ot)){

				rho = MATCH_PART(check_Rq, Ntp1);
				*prob = *prob + prob_Y1[rho] * prob_Hq;

				// debug
				// Rprintf("R_%d is:", q);
				// print_int(Rq, Ot);
				// Rprintf("check_R_%d is:", q);
				// print_int(check_Rq, Ntp1);
				// Rprintf("rho = %d\n", rho);
				// Rprintf("prob_1=%f, prob_Y1 = %f, prob_Hq = %f\n",*prob, prob_Y1[rho], prob_Hq);
						
				}
					
			
		}//the end of loop for q

		delete[] Hq;
		delete[] Rtemp;
		delete[] Rq;
		delete[] check_Rq;
		UNPROTECT(nProtect);
		return res;
	}

}
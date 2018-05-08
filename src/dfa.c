//
//  DFA.c skeleton code
//  Design 2 uses multi dim array to implement dfas
//
//  Created by Lalka 3/21/2018
//

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#define ASCII_LEN 94

static char ASCII [] = {33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126}; 

struct dfa_t{
  /*
    Q is a finite set of states.
    ∑ is a finite set of symbols called the alphabet.
    δ is the transition function where δ: Q × ∑ → Q
    q0 is the initial state from where any input is processed (q0 ∈ Q).
    F is a set of final state/states of Q (F ⊆ Q).
  */
  int nstates;
  char *alphabet;
  int nsym;
  int init;//by state id
  int *final;//by state id
  int nfin;
  int * delta;
};


//DFA functions

int idx(int i, int j, int nsym) {
  return (i*nsym+j);
}

int * makeDelta(int len, char * sym, int nsym){
  int * sts = malloc(len*nsym*(sizeof(int)));
  for(int i = 0; i < len; i++){
    for(int j = 0; j < nsym; j++){
      sts[idx(i, j, nsym)] = -1;
    }
  }
  return sts;
}

//constructor
struct dfa_t construct(int nstates, char * alphabet, int nsym, int init, int *final, int nfin){
  struct dfa_t self;
  self.nstates = nstates;
  char * alpha = malloc(nsym*sizeof(char));
  for(int i = 0; i < nsym; i++){
    alpha[i]= alphabet[i];
    printf("Adding chracter %c\n", alpha[i]);
  }
  self.alphabet = alpha;
  self.nsym = nsym;
  self.init = init;
  int * fin = malloc(nfin*sizeof(int));
  for(int i = 0; i < nfin; i++){
    fin[i]= final[i];
  }
  self.final = fin;
  self.nfin = nfin;
  self.delta = makeDelta(nstates, alphabet, nsym);
  return self;
}


// free memory associated with DFA
// assume final and alphabet were malloc'd
void destruct(struct dfa_t d){
  if(d.delta) {
    free(d.delta);
  }

 if(d.alphabet) {
    free(d.alphabet);
  }

  if(d.final){
    free(d.final);
  }
  return;
}

// helper function
//returns if state is in dfa
int hasState(struct dfa_t *dfa, int id){
  if(id < dfa->nstates){
    return 1;
  }
  return 0;
}

//returns -1 if the character is not in the alphabet of the dfa
int IntOfSymbol(struct dfa_t *dfa, char sym){
  for(int i = 0; i < dfa->nsym; i++){
    if(sym == dfa->alphabet[i]){
      return i;
    }
  }
  return -1;
}

int link(struct dfa_t *dfa, int from, int to, char sym){
  int sym_pos = IntOfSymbol(dfa, sym);
  if(hasState(dfa, from) && hasState(dfa, to) && (sym_pos != -1)){
    printf("Linkng (%i, %c) -> %i || %i\n", from, sym, to, sym-1);
    dfa->delta[idx(from, sym_pos, dfa->nsym)]=to;
    return 1;
  }
  return 0;
}

/*
int link(struct dfa_t dfa, int from, int to, char sym){
  if(hasState(dfa, from) && hasState(dfa, to) && hasSymbol(dfa, sym)){
    return linkStruct(dfa, hasState(dfa, from), hasState(dfa, to), sym);
  }
  return 0;
}*/

// follows transition from s on input
// returns a pointer to the resulting state
// returns -1 when no transition exists
int transition(struct dfa_t *d, int curr_state, char input){
  int sym_pos = IntOfSymbol(d, input);
  if(hasState(d, curr_state && (sym_pos != -1))){
    return d->delta[idx(curr_state, sym_pos, d->nsym)];
  }
  return -1;
}

//takes list of characters and returns a result state
int simulates(struct dfa_t *dfa, char *input){
  int nxt = dfa->init;
  for(int i = 0; i < strlen(input); i++){
    if(nxt != -1) {
      nxt = transition(dfa, nxt, input[i]);
    } else {
     // printf("Sent to failure state\n");
      return -1;
    }
  }
  return nxt;
}

//Simluates input on dfa
//returns 1 if accepted
//0 if not accepted

int accepts(struct dfa_t * dfa, char *input){
  int check = simulates(dfa, input);
  for(int i = 0; i < dfa->nfin; i++){
    if(dfa->final[i] == check){
      return 1;
    }
  }
  return 0;    
}


int printdfa_compact(struct dfa_t * d){
  // print states
  printf("\nnstates:  %i", d->nstates);
  // alphabet 
  printf("\nalphabet: ");
  for(int i = 0; i < d->nsym; i++){
    printf("%c ", d->alphabet[i]);
  }
  printf("\nnsym:     %i", d->nsym);
  // start state
  printf("\nstart:    %i", d->init);
  // accept states
  printf("\nfin:      ");
  for(int i = 0; i < d->nfin; i++){
    printf("%i ", d->final[i]);
  }
  printf("\nnfin:     %i", d->nfin);
  printf("\n");

  return 0;
}

int printdfa(struct dfa_t * d){

  // print states
  printf("\nnstates:  %i", d->nstates);

  // alphabet 
  printf("\nalphabet: ");
  for(int i = 0; i < d->nsym; i++){
    printf("%c ", d->alphabet[i]);
  }
  printf("\nnsym:     %i", d->nsym);

  // start state
  printf("\nstart:    %i", d->init);

  // accept states
  printf("\nfin:      ");
  for(int i = 0; i < d->nfin; i++){
    printf("%i ", d->final[i]);
  }
  printf("\nnfin:     %i", d->nfin);

  // delta function displayed in funciton table
  printf("\ndelta:    \n");
  printf("        ");
  for(int i = 0; i < d->nsym; i++){
    printf("|%4c ", d->alphabet[i]);
  }
  printf("\n    ----");
  for(int i = 0; i < d->nsym; i++){
    printf("|-----");
  }
  for(int i = 0; i < d->nstates; i++){
     printf("\n    %4i|", i);
    for(int j = 0; j < d->nsym; j++){
      printf("%4i  ", d->delta[i*d->nsym+j]);
    }
  }

  printf("\n");

  return 0;
}

int reindex(int i, int j, int n1, int n2){
  if(i == -1){
    if(j == -1){
      return (n1+1)*(n2+1) -1;
    } else {
      return n1*(n2+1) + j;
    }
  }
  if(j == -1){
    return n2*n1+i;
  }

  return i*n2+j;
}

int search(int * list, int len, int key){
  for(int i = 0; i < len; i++){
    if(list[i] == key){
      return 1;
    }
  }
  return 0;
}

int dfaunion(struct dfa_t * d1, struct dfa_t * d2, struct dfa_t * result){
  int n1 = d1->nstates;
  int n2 = d2->nstates;

  //set initial state
  result->init = reindex(d1->init, d2->init, n1, n2);

  //copy alphabet
  for(int i = 0; i < result->nsym; i++){
    result->alphabet[i] = d2->alphabet[i];
  }

  //set the final states
  int fin_index = 0;
  result->init = reindex(d1->init, d2->init,n1, n2);
  for(int i =0; i < d1->nfin; i++){
    for(int j=0; j < n2; j++){
      result->final[fin_index] = reindex(d1->final[i],j,n1, n2);
      fin_index++;
    }
    result->final[fin_index] = reindex(d1->final[i],-1,n1, n2);
    fin_index++;
  }
  for(int i =0; i < d2->nfin; i++){
    for(int j=0; j < n1; j++){
      if(!search(d1->final, d1->nfin, j)){
        result->final[fin_index] = reindex(j,d2->final[i],n1, n2);
        fin_index++;
      }
    }
    result->final[fin_index] = reindex(-1,d2->final[i],n1, n2);
    fin_index++;
  }

  /*** build the transition function ***/
  // start with setting everything -1 for no transition
  for(int r = 0; r < result->nstates; r++){
    for(int s = 0; s < result->nsym; s++){
      result->delta[idx(r, s, result->nsym)] = -1;
    }
  }

  for(int i = 0; i < n1; i++){
    for(int j =0; j < n2; j++){
      int from = reindex(i, j, n1, n2);
      for(int k=0; k < result->nsym; k++){
        int to = -1;
        int d1_to = d1->delta[idx(i,IntOfSymbol(d1, result->alphabet[k]), d1->nsym)];
        int d2_to = d2->delta[idx(j,IntOfSymbol(d2, result->alphabet[k]), d2->nsym)];
        to = reindex(d1_to, d2_to, n1, n2);
        link(result, from, to, result->alphabet[k]);
      }
    }
  }
  for(int i = 0; i < n1; i++){
    int from = reindex(i, -1, n1, n2);
    for(int k=0; k < result->nsym; k++){
        int d1_to = d1->delta[idx(i,IntOfSymbol(d1, result->alphabet[k]), d1->nsym)];
        int to = reindex(d1_to, -1, n1, n2);
        link(result, from, to, result->alphabet[k]);
    }
  }
    for(int j = 0; j < n2; j++){
    int from = reindex(-1, j, n1, n2);
    for(int k=0; k < result->nsym; k++){
        int d2_to = d2->delta[idx(j,IntOfSymbol(d2, result->alphabet[k]), d2->nsym)];
        int to = reindex(-1, d2_to, n1, n2);
        link(result, from, to, result->alphabet[k]);
    }
  }
  return 0;
}

int len(const char * str){
  return strlen(str);
}

#ifdef BUILD_TEST
int main(){
  printf("Test3---------\n");
  int Q [] = {0,1,2,3,4,5};
  int F [] = {4,5};
  struct dfa_t test3 = construct(6, ASCII, ASCII_LEN, 1, F, 2);

  printf("Links...\n");
  assert(link(&test3, 1, 2, 'e') != 0);
  assert(link(&test3, 2, 3, 'l') != 0);
  assert (link(&test3, 3, 4, 's') != 0);
  assert (link(&test3, 4, 5, 'e') != 0);
  assert (link(&test3, 5, 5, '$') != 0);

  assert (link(&test3, 5, 5, 126) != 0);
  printf("PASS\n");

  printf("Transitions...\n");
  assert(transition(&test3, 1, 'e') == 2);
  assert(transition(&test3, 1, 'q') == -1);
  printf("PASS\n");

  printf("Evaluate...\n");
  assert(simulates(&test3, "els") == 4);
  assert(simulates(&test3, "xxx") == -1);
  assert(simulates(&test3, "") == 1);
  printf("PASS\n");

  printf("Accept...\n");
  assert(accepts("els", &test3));
  assert(accepts("else", &test3));
  assert(accepts("else$$$$$$$$$$$$$$$$$$$", &test3));
  assert(accepts("else$$$$$$$$$$$$$$$$~~$", &test3));
  assert(!accepts("elsf", &test3));
  printf("Pass\n");

  printdfa_compact(&test3);

  destruct(test3);
  
  return 0;
}
#endif
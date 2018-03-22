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

#define ASCII_LEN 126

static char ASCII [] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255};

struct dfa_t{
  /*
    Q is a finite set of states.
    ∑ is a finite set of symbols called the alphabet.
    δ is the transition function where δ: Q × ∑ → Q
    q0 is the initial state from where any input is processed (q0 ∈ Q).
    F is a set of final state/states of Q (F ⊆ Q).
  */
  int nstates;
  int init;//by state id
  int *final;//by state id
  int nfin;
  int ** delta;
};


//DFA functions

// Input: list of int ids, number of ids, alphabet and size of alphabet
//Output: array of dfa_states with associated ids; has transitions initialized to NULL
/*
struct dfa_state * makeStates(int *ids, int len, char * alpha, int nsym){
  struct dfa_state * sts = malloc(len*sizeof(struct dfa_state));
  for(int i = 0; i < len; i++){
    sts[i].ID= ids[i];
    sts[i].links = malloc(nsym*sizeof(struct transition));
    for(int j = 0; j < nsym; j++){
      sts[i].links[j].input = alpha[j];
      sts[i].links[j].next = NULL;
      sts[i].nlinks = nsym;
    }
  }

  return sts;
}
*/

int ** makeDelta(int len){
  int ** sts = malloc(len*(sizeof(int *)));
  for(int i = 0; i < len; i++){
    sts[i] = malloc(ASCII_LEN*sizeof(int));
    for(int j = 0; j < ASCII_LEN; j++){
      sts[i][j] = -1;
    }
  }
  return sts;
}

//constructor
//TODO: check final and init are in states
struct dfa_t construct(int nstates, int init, int *final, int nfin){
  struct dfa_t self;
  self.nstates = nstates;
  self.init = init;
  int * fin = malloc(nfin*sizeof(int));
  for(int i = 0; i < nfin; i++){
    fin[i]= final[i];
  }
  self.final = fin;
  self.nfin = nfin;
  self.delta = makeDelta(nstates);
  return self;
}

// free memory associated with DFA
// assume final and alphabet were malloc'd
void destruct(struct dfa_t d){
  for(int i = 0; i < d.nstates; i++){
    free(d.delta[i]);
  }
  if(d.delta) {
    free(d.delta);
  }

  if(d.final){
    free(d.final);
  }
  return;
}

// helper function
//returns if state is in dfa
int hasState(struct dfa_t dfa, int id){
  if(id < dfa.nstates){
    return 1;
  }
  return 0;
}

int link(struct dfa_t dfa, int from, int to, char sym){
  if(hasState(dfa, from) && hasState(dfa, to)){
    printf("Linkng (%i, %c) -> %i || %i\n", from, sym, to, sym-1);
    dfa.delta[from][sym-1]=to;
    return 1;
  }
  return 0;
}

// follows transition from s on input
// returns a pointer to the resulting state
// returns null when no transition exists
int transition(struct dfa_t d, int curr_state, char input){
  if(hasState(d, curr_state)){
    return d.delta[curr_state][input-1];
  }
  return -1;
}

//takes list of characters and returns a result state
int evaluate(struct dfa_t dfa, char *input){

  int nxt = dfa.init;
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
int accept(struct dfa_t dfa, char *input){
  int check = evaluate(dfa, input);
  for(int i = 0; i < dfa.nfin; i++){
    if(dfa.final[i] == check){
      return 1;
    }
  }
  return 0;    
}

int main(){

  printf("Test3---------\n");
  int Q [] = {0,1,2,3,4,5};
  int F [] = {4,5};
  struct dfa_t test3 = construct(6, 1, F, 2);

  assert(test3.delta[0][0] == -1);

  printf("Links...\n");
  assert(link(test3, 1, 2, 'e') != 0);
  assert(link(test3, 2, 3, 'l') != 0);
  assert (link(test3, 3, 4, 's') != 0);
  assert (link(test3, 4, 5, 'e') != 0);
  assert (link(test3, 5, 5, '$') != 0);
  assert (link(test3, 5, 5, 126) != 0);
  printf("PASS\n");

  printf("Transitions...\n");
  assert(transition(test3, 1, 'e') == 2);
  assert(transition(test3, 1, 'q') == -1);
  printf("PASS\n");

  printf("Evaluate...\n");
  assert(evaluate(test3, "els") == 4);
  assert(evaluate(test3, "xxx") == -1);
  assert(evaluate(test3, "") == 1);
  printf("PASS\n");

  printf("Accept...\n");
  assert(accept(test3, "els"));
  assert(accept(test3, "else"));
  assert(accept(test3, "else$$$$$$$$$$$$$$$$$$$"));
  assert(accept(test3, "else$$$$$$$$$$$$$$$$~~$"));
  assert(!accept(test3, "elsf"));
  printf("Pass\n");

  destruct(test3);
  
  return 0;
}

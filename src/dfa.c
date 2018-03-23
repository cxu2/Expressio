//
//  DFA.c skeleton code
//  Design 1 uses poniters to implement dfas
//  as a directed graph
//
//  Created by David Han on 2/11/18.
//

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

static char ASCII [] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255};

struct dfa_state{
  int ID;
  struct transition *links;
  int nlinks;
  
};

struct transition{ //embed transition function into states via links
  char input;
  struct dfa_state* next;
};

struct dfa_t{
  /*
    Q is a finite set of states.
    ∑ is a finite set of symbols called the alphabet.
    δ is the transition function where δ: Q × ∑ → Q
    q0 is the initial state from where any input is processed (q0 ∈ Q).
    F is a set of final state/states of Q (F ⊆ Q).
  */
  struct dfa_state *states;//points to collection of states
  int nstates;
  char *alphabet;
  int nsym;
  int init;//by state id
  int *final;//by state id
  int nfin;
};


//DFA functions

// Input: list of int ids, number of ids, alphabet and size of alphabet
//Output: array of dfa_states with associated ids; has transitions initialized to NULL
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

//constructor
//TODO: check final and init are in states
struct dfa_t construct(int *ids, int nstates, char * alphabet, int nsym, int init, int *final, int nfin){
  struct dfa_t self;
  struct dfa_state * states = makeStates(ids, nstates, alphabet, nsym);
  self.states = states;
  self.nstates = nstates;
  char * alpha = malloc(nsym*sizeof(char));
  for(int i = 0; i < nsym; i++){
    alpha[i]= alphabet[i];
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

  return self;
}

// free memory associated with DFA
// assume final and alphabet were malloc'd
void destruct(struct dfa_t d){
  for(int i = 0; i < d.nstates; i++){
    free(d.states[i].links);
  }
  if(d.states) {
    free(d.states);
  }
  if(d.alphabet){
    free(d.alphabet);
  }
  if(d.final){
    free(d.final);
  }
  return;
}

// helper function
//returns pointer to state with id if it is in the dfa
struct dfa_state * hasState(struct dfa_t dfa, int id){
  for(int i = 0; i < dfa.nstates; i++){
    if(id == dfa.states[i].ID){
      return &(dfa.states[i]);
    }
  }
  return NULL;
}


//helper function returns true if sym is in the dfa's alphabet
int hasSymbol(struct dfa_t dfa, char sym){
  for(int i = 0; i < dfa.nsym; i++){
    if(sym == dfa.alphabet[i]){
      return 1;
    }
  }
  return 0;
}

//adds a transition from 'from' to 'to' on input symbol sym
// if sym is not in the alphbet, returns 0
// on success, returns 1
int linkStruct(struct dfa_t dfa, struct dfa_state* from, struct dfa_state* to, char sym){
  if(hasState(dfa, from->ID) && hasState(dfa, to->ID) && hasSymbol(dfa, sym)){
    for(int i = 0; i < dfa.nsym; i++){
      if(sym == from->links[i].input){
	      from->links[i].next = to;
	      return 1;
      }
    }
  }
  return 0;
}

int link(struct dfa_t dfa, int from, int to, char sym){
  if(hasState(dfa, from) && hasState(dfa, to) && hasSymbol(dfa, sym)){
    return linkStruct(dfa, hasState(dfa, from), hasState(dfa, to), sym);
  }
  return 0;
}

// follows transition from s on input
// returns a pointer to the resulting state
// returns null when no transition exists
struct dfa_state *transition(struct dfa_state *s, char input){
  for(int i = 0; i < s->nlinks; i++){
    if(s->links[i].input == input){
    //  printf("Transition from state %i\n",s->ID);
      if(s->links[i].next) {
      //  printf("SOMETHING");
        return s->links[i].next;
      } else {
       // printf("NOTHING");
        return NULL;
      }
    }
  }
  return NULL; //not usually reached
}

//takes list of characters and returns a result state
struct dfa_state *evaluate(struct dfa_t dfa, char *input){

  struct dfa_state * nxt = hasState(dfa, dfa.init);
  for(int i = 0; i < strlen(input); i++){
    if(nxt) {
   //   printf("String access %i\n", i);
    //  printf("transition from %i on %c\n", nxt->ID, input[i]);
      nxt = transition(nxt, input[i]);
    //  printf("Returned?\n");
    } else {
     // printf("Sent to failure state\n");
      return NULL;
    }
  }
  return nxt;
}


//Simluates input on dfa
//returns 1 if accepted
//0 if not accepted
int accept(struct dfa_t dfa, char *input){
  struct dfa_state * check = evaluate(dfa, input);
  if(check){
    for(int i = 0; i < dfa.nfin; i++){
      if(dfa.final[i] == check->ID){
        return 1;
      }
    }
  }
  return 0;    
}

#ifdef BUILD_TEST
int main(){
  char *alpha = malloc(2*sizeof(char));
  alpha[0] = '0';
  alpha[1] = '1';
  int ids[] = {3,2,1};

  printf("Test2---------\n");

  int *fin = malloc(sizeof(int));
  fin[0] = 1;

  struct dfa_t test2 = construct(ids, sizeof(ids)/sizeof(int), alpha, 2, 3, fin, 1);
  
  for(int i = 0; i < sizeof(ids)/sizeof(int); i++){
    if(hasState(test2, ids[i])){
      printf("Has st %i\n", ids[i]);
    } else {
      printf("FAIL: NOT has st %i\n", ids[i]);
    }  
  }

  if(hasState(test2, 12)){
    printf("FAIL: has st %i\n", 12);
  } else {
    printf("NOT Has 12\n");
  }


  
  link(test2, 3, 2, '0');
  link(test2, 2, 1, '1');
  link(test2, 1, 2, '1');

  printf("3->2 on 0: %i\n", (transition(hasState(test2, 3), '0'))->ID);
  if(transition(hasState(test2, 3), '1')){
    printf("FAIL 3 on 1: Should return failure state\n");
  } else {
    printf("3 on 1: Goes to failure state\n");
  }

  if(transition(hasState(test2, 2), '0')){
    printf("FAIL 2 on 0: Should return failure state\n");
  } else {
    printf("2 on 0: Goes to failure state\n");
  }

  printf("2->1 on 1: %i\n", (transition(hasState(test2, 2), '1'))->ID);

  if(transition(hasState(test2, 1), '0')){
    printf("FAIL 1 on 0: Should return failure state\n");
  } else {
    printf("1 on 0: Goes to failure state\n");
  }

  printf("1->2 on 1: %i\n", (transition(hasState(test2, 1), '1'))->ID);

  printf("Accepts 01?: %i\n", accept(test2, "01"));

  printf("Accepts 00?: %i\n", accept(test2, "00"));

  printf("Accepts xx?: %i\n", accept(test2, "xx"));

  destruct(test2);
  free(alpha);
  free(fin);

  printf("Test3---------\n");
  int Q [] = {1,2,3,4,5};
  int F [] = {4,5};
  struct dfa_t test3 = construct(Q, 5, ASCII, 255, 1, F, 2);

  printf("Links...\n");
  assert(link(test3, 1, 2, 'e') != 0);
  assert(link(test3, 2, 3, 'l') != 0);
  assert (link(test3, 3, 4, 's') != 0);
  assert (link(test3, 4, 5, 'e') != 0);
  assert (link(test3, 5, 5, '$') != 0);
  printf("Pass\n");
  printf("Accept...\n");
  assert(accept(test3, "els"));
  assert(accept(test3, "else"));
  assert(accept(test3, "else$$$$$$$$$$$$$$$$$$$"));
  assert(!accept(test3, "elsf"));
  printf("Pass\n");
  destruct(test3);
  
  return 0;
}
#endif
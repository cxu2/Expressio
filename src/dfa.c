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
  struct dfa_state *init;
  struct dfa_state **final;
  int nfin;
};


//DFA functions
struct dfa_t construct(struct dfa_state * states, int nstates, char * alphabet, int nsym, struct dfa_state *init, struct dfa_state **final, int nfin){
  struct dfa_t self;
  self.states = states;
  self.nstates = nstates;
  self.alphabet = alphabet;
  self.nsym = nsym;
  self.init = init;
  self.final = final;
  self.nfin = nfin;

  return self;
}

// free memory
void destruct(struct dfa_t d){
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

struct dfa_state * makeStates(int *ids, int len, char * alpha, int nsym){
  struct dfa_state * sts = malloc(len*sizeof(struct dfa_state));
  for(int i = 0; i < len; i++){
    sts[i].ID= ids[i];
    sts[i].links = malloc(nsym*sizeof(struct transition));
    for(int j = 0; j < nsym; j++){
      sts[i].links[j].input = alpha[i];
    }
  }

  return sts;
}

struct dfa_state * hasState(struct dfa_t dfa, int id){
  for(int i = 0; i < dfa.nstates; i++){
    if(id == dfa.states[i].ID){
      return &(dfa.states[i]);
    }
  }
  return NULL;
}

int hasSymbol(struct dfa_t dfa, char sym){
  for(int i = 0; i < dfa.nsym; i++){
    if(sym == dfa.alphabet[i]){
      return 1;
    }
  }
  return 0;
}

int link(struct dfa_t dfa, struct dfa_state* from, struct dfa_state* to, char sym){
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

struct dfa_state *transition(struct dfa_state s, char input){ return NULL;}

//takes list of characters and returns a result state
struct dfa_state *evaluate(struct dfa_t dfa, char *input){
  
  for(int i = 0;i < strlen(input); i++){
    //do transition for each char
    ;
  }
  return NULL;
}



int accept(struct dfa_t dfa, char *input){
  struct dfa_state * check = evaluate(dfa, input);
  return 0;    
}


int main(){
  struct dfa_t test;
  char alpha[] = {'0','1'};
  int ids[] = {3,2,1};
  struct dfa_state * sts = makeStates(ids, sizeof(ids)/sizeof(int), alpha, 2);
  test.states = sts;
  test.nstates = 3;
  
  for(int i = 0; i < sizeof(ids)/sizeof(int); i++){
    printf("Has st %i: %i\n", ids[i], hasState(test, ids[i]));
  }
  
  printf("Has 12: %i\n", hasState(test, 12));

  link(test, hasState(test, 3), hasState(test,2), '0');

  return 0;
}

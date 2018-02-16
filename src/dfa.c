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
  int init;//by state id
  int *final;//by state id
  int nfin;
};


//DFA functions

//constructor
//TODO: check final and init are in states
struct dfa_t construct(struct dfa_state * states, int nstates, char * alphabet, int nsym, int init, int *final, int nfin){
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
// assume final and alphabet were malloc'd
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
      sts[i].nlinks = nsym;
    }
  }

  return sts;
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

// follows transition from s on input
// returns a pointer to the resulting state
struct dfa_state *transition(struct dfa_state *s, char input){
  for(int i = 0; i < s->nlinks; i++){
    if(s->links[i].input == input){
      return s->links[i].next;
    }
  }
  return s;
}

//takes list of characters and returns a result state
struct dfa_state *evaluate(struct dfa_t dfa, char *input){

  struct dfa_state * nxt = hasState(dfa, dfa.init);
  for(int i = 0; i < strlen(input); i++){
    printf("transition from %i on %c\n", nxt->ID, input[i]);
    nxt = transition(nxt, input[i]);
  }
  printf("ended on %i\n", nxt->ID);
  return nxt;
}


//Simluates input on dfa
//returns 1 if accepted
//0 if not accepted
int accept(struct dfa_t dfa, char *input){
  struct dfa_state * check = evaluate(dfa, input);
  for(int i = 0; i < dfa.nfin; i++){
    if(dfa.final[i] == check->ID){
      return 1;
    }
  }
  return 0;    
}


int main(){
  struct dfa_t test;
  char *alpha = malloc(2*sizeof(char));
  alpha[0] = '0';
  alpha[1] = '1';
  int ids[] = {3,2,1};
  struct dfa_state * sts = makeStates(ids, sizeof(ids)/sizeof(int), alpha, 2);
  test.states = sts;
  test.nstates = 3;
  
  for(int i = 0; i < sizeof(ids)/sizeof(int); i++){
    printf("Has st %i: %i\n", ids[i], hasState(test, ids[i]));
  }
  
  printf("Has 12: %i\n", hasState(test, 12));

  printf("Test2---------\n");

  int *fin = malloc(sizeof(int));
  fin[0] = 1;

  struct dfa_t test2 = construct(sts, 3, alpha, 2, 3, fin, 1);

  for(int i = 0; i < sizeof(ids)/sizeof(int); i++){
    printf("Has st %i: %i\n", ids[i], hasState(test, ids[i]));
  }

  printf("Has 12: %i\n", hasState(test, 12));

  link(test2, hasState(test2, 3), hasState(test2, 2), '0');
  link(test2, hasState(test2, 2), hasState(test2, 1), '1');
  link(test2, hasState(test2, 1), hasState(test2, 2), '1');
  
  printf("3->2 on 0: %i\n", (transition(hasState(test2, 3), '0'))->ID);
  printf("3 on 1: %i\n", (transition(hasState(test2, 3), '1'))->ID);
  printf("2 on 0: %i\n", (transition(hasState(test2, 2), '0'))->ID);
  printf("2->1 on 1: %i\n", (transition(hasState(test2, 1), '1'))->ID);
  printf("1 on 0: %i\n", (transition(hasState(test2, 1), '0'))->ID);
  printf("1->2 on 1: %i\n", (transition(hasState(test2, 1), '1'))->ID);

  printf("Accepts 01?: %i\n", accept(test2, "01"));
  printf("Accepts 00?: %i\n", accept(test2, "00"));
  printf("Accepts xx?: %i", accept(test2, "xx"));
  
  destruct(test2);
  
  return 0;
}

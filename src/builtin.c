/*
 * File: codegen.ml
 * Date: 2018-03-26
 *
 * PLT Spring 2018
 * Expressio Project
 * Ian Treyball      <ict2102@columbia.edu>
 * Lalka Rieger      <ler2161@columbia.edu>
 * Chengtian Xu      <cx2168@columbia.edu>
 * David Han         <dth2126@columbia.edu>
 */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct {
  char operator;
  char character;
  char * left;
  char * right;
} tree_t;

int printr(tree_t* regex_ptr) {
	if (regex_ptr -> operator == 'l') {
    printf("%c", regex_ptr -> character);
  } else if (regex_ptr -> operator == '*' || regex_ptr -> operator == '\\') {
    printr((tree_t *)(regex_ptr -> left));
    printf(" %c ", regex_ptr -> operator); 
  } else {
    printf("( ");
    printr((tree_t *)(regex_ptr -> left));
    printf(" %c ", regex_ptr -> operator);  
    printr((tree_t *)(regex_ptr -> right));
    printf(" )");
  }
 
	return 0;
  // printf("tree: %p\n", regex_ptr);
  // printf("operator: %c\n", regex_ptr -> operator);
  // printf("left: %p\n", regex_ptr -> left);
  // printf("right: %p\n", regex_ptr -> right);
  // printf("left character: %c\n", ((tree_t *)(regex_ptr -> left)) -> character);
  // printf("right character: %c\n", ((tree_t *)(regex_ptr -> right)) -> character);
}

tree_t* clone(tree_t* regex_ptr) {
  tree_t* t = (tree_t*)malloc(sizeof(tree_t));
  return t;
}

int identical(tree_t* r1, tree_t* r2) {
  if (r1 == NULL && r2 == NULL)
    return 1;

  if (r1 != NULL && r2 != NULL) {
    return (r1 -> operator == r2 -> operator &&
            r1 -> character == r2 -> character &&
            identical((tree_t*)r1 -> left, (tree_t*)r2 -> left) &&
            identical((tree_t*)r1 -> right, (tree_t*)r2 -> right));
  }

  return 0;
}


tree_t* Zero(){
  tree_t* t = (tree_t*)malloc(sizeof(tree_t));
  if (t == NULL) {
    printf("malloc failed due to out of memory.");
    exit(1);
  }
  t -> operator = 'n';
  t -> character = '@';
  return t;
}

tree_t* One() {
  tree_t* t = (tree_t*)malloc(sizeof(tree_t));
  if (t == NULL) {
    printf("malloc failed due to out of memory.");
    exit(1);
  }
  t -> operator = 'n';
  t -> character = '#';
  return t;
}

tree_t* Mult(tree_t* left, tree_t* right) {
  tree_t* t = (tree_t*)malloc(sizeof(tree_t));
  if (t == NULL) {
    printf("malloc failed due to out of memory.");
    exit(1);
  }
  t -> operator = '^';
  t -> left = &left -> operator;
  t -> right = &right -> operator;
  return t;
}

tree_t* Plus(tree_t* left, tree_t* right) {
  tree_t* t = (tree_t*)malloc(sizeof(tree_t));
  if (t == NULL) {
    printf("malloc failed due to out of memory.");
    exit(1);
  }
  t -> operator = '|';
  t -> left = &left -> operator;
  t -> right = &right -> operator;
  return t;
}

tree_t* Star(tree_t* regex) {
  tree_t* t = (tree_t*)malloc(sizeof(tree_t));
  if (t == NULL) {
    printf("malloc failed due to out of memory.");
    exit(1);
  }
  t -> operator = '|';
  t -> left = &regex -> operator;
  t -> right = NULL;
  return t;
}

tree_t* mult(tree_t* r1, tree_t* r2) {
  if (r2 -> operator == 'n' && r2 -> character == '#'){
    return r2;
  } else if (r1 -> operator == 'n' && r1 -> character == '#') {
    return r1;
  } else if (r2 -> operator == 'n' && r2 -> character == '@') {
    return r1;
  } else if (r1 -> operator == 'n' && r1 -> character == '@') {
    return r2;
  } else if (r1 -> operator == '^') {
    return Mult((tree_t*)(r1 -> left), mult((tree_t*)(r1 -> right), r2));
  } else {
    return Mult(r1, r2);
  }
}

tree_t* plus(tree_t* r1, tree_t* r2) {
  return r1;
  // if (r2 -> operator == 'n' && r2 -> character == '#'){
  //   return r1;
  // } else if (r1 -> operator == 'n' && r1 -> character == '#') {
  //   return r2;
  // } else if (r1 -> operator == '|') {
  //   return plus((tree_t*)r1 -> left, plus((tree_t*)r1 -> right, r2));
  // } else if (r2 -> operator == '|') {
  //   if (identical(r1, (tree_t*)r2 -> left)) {
  //     return Plus((tree_t*)r2 -> left, (tree_t*)r2 -> right);
  //   } else {

  //   }

  // }
}

tree_t* star(tree_t* r) {
  return r;
}

tree_t* constant(tree_t* r) {
  return r;
}

tree_t* derivative(tree_t* regex_ptr, char c) {
  switch(regex_ptr -> operator) {
    // Zero or One
    case 'n':
      return regex_ptr -> character == '#' ? Zero() : One();
    // Lit
    case 'l':
      return regex_ptr -> character == c ? One() : Zero();
    // Star
    case '*':
      return mult(derivative((tree_t*)(regex_ptr -> left), c), star((tree_t*)(regex_ptr -> left)));
    // Plus
    case '|':
      return plus(derivative((tree_t*)(regex_ptr -> left), c), derivative((tree_t*)(regex_ptr -> right), c));
    // Mult
    case '^':
      return plus(mult(derivative((tree_t*)(regex_ptr -> left), c), (tree_t*)regex_ptr -> right), 
        mult(constant((tree_t*)(regex_ptr -> left)), derivative((tree_t*)(regex_ptr -> right), c)));
    default:
      return regex_ptr -> character == '#' ? Zero() : One();
  }
}

int matchesHelper(tree_t* regex_ptr, char* str) {
  if (regex_ptr -> operator == 'n' && regex_ptr -> character == '#') {
    free(regex_ptr);
    return 0;
  }

  // empty string matches only with epsilon
  if (str[0] == '\0') {
    if (regex_ptr -> operator == 'n' && regex_ptr -> character == '@') {
      free(regex_ptr);
      return 1;
    }
  }

  char curChar = str[0];
  return matchesHelper(derivative(regex_ptr, curChar), str++);
}

int matches(tree_t* regex_ptr, char* str) {
  // empty language does not match with any string
  return matchesHelper(regex_ptr, str);
}























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
	if (regex_ptr == NULL) {
		return 0;
  }
 
	printf("( ");
  printr((tree_t *)(regex_ptr -> left));

  printf("%c", regex_ptr -> character);
  printf("%c", regex_ptr -> operator);  

  printr((tree_t *)(regex_ptr -> right));
  printf(" )");

  return 0;
}
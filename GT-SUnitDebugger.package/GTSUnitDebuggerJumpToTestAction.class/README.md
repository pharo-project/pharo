I implement a debugging action for selecting in the stack the context containing the test  method.
  
I am only valid if the context that triggered the failed assertion is diffferent than the context containing the test method (e.g., the assertion method was called from an utility method of the test)
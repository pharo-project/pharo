IRClosureStackCount is used to distinguish between a stack 
in the method scope and a stack within a closure block. The
closure stack size is independent of the number of tempvars from the compiled method, therefore that number is subtracted
from this stack size length.
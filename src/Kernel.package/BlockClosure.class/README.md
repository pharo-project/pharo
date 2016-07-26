I contain a sequence of operations. I am defined by Smalltalk expressions inside square brackets. I permit to defer the enclosed operations until I execute a variant of #value. I can have my own arguments and temporaries as a regular method, but I am also able to use external variables: my enclosing method or block temporaries, arguments and receiver.

examples :
[ 1 + 2 ] value
[ :arg | 
	| temp | 
	temp := arg. 
	temp ] value: 5
[ ^ 5 ] value

My return value corresponds to my final expression. A non local return (^) has the same effect as if I did not exist: it returns from my enclosing method, even if I'm nested in other blocks. 

Implementation:

Instance variables:
	outerContext <Context|nil> context that defined me
	startpc: <SmallInteger> (pc = program counter) offset of my first bytecode instruction in the compiledMethod bytecode  
	numArgs: <SmallInteger> my number of arguments

I am created at runtime through a special bytecode:
closureNumCopied: x numArgs: y bytes z1 to z2
On creation, the currently executed context is set to my outerContext, z1 is set as my startpc and y is set as my numArgs. After my creation, the current execution flow jumps to my last bytecode, z2, to skip the execution of my bytecode which is deferred until I execute a variant of #value.

I am executed when I receive a variant of the message value. This message creates a new context, a block context <MethodContext>, which reference me in its variable closureOrNil. This new context executes my bytecode, which correspond to a subset of the bytecode of my enclosing method, starting at startpc and ending in blockReturn/return bytecode.

Accessing variables of the my enclosing context is different depending on variables because of various optimizations:
- self: I access the receiver of my enclosing method by accessing my context's receiver, which is always set to the enclosing method receiver.
- copied variables: If I read a variable from an outerContext but I don't write into it and the variable is not modified after the BlockClosure creation, then the variable is copied in the blockClosure to be more efficient. 
- full variable: If I access and edit a variable from an outerContext, then the variable is stored in an external heap allocated array (named tempVector). The tempVector is known by the method and the block so they can both read and write these variables.

Optimized block closures: 
Common blocks (2/3 of the blocks) are optimized directly in the compiler and have special behaviors. These blocks are the arguments/receiver of control structures: #ifNil:, #ifNotNil:, #ifTrue:, #ifFalse:, #whileTrue:, #whileFalse:, #to:do:, #to:by:do: .

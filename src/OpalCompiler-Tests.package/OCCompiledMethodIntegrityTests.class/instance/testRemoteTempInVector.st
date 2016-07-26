test
testRemoteTempInVector

	| newCompiledMethod |

	"Here the problem was that the Scope kept both the remote temp answer and the new remote <?> this caused
	that the number of temps were more than the correnct"
	
	newCompiledMethod := OpalCompiler new 
											source: 'value
													    | answer |
 													   self do:
       														 [:each |
        															answer := each value].
   													 ^answer';
											class: Object;
											compile.

							
	self assert: newCompiledMethod numArgs = 0.
	self assert: newCompiledMethod numLiterals = 2.
	self assert: newCompiledMethod numTemps = 1.
	self assert: newCompiledMethod primitive = 0.
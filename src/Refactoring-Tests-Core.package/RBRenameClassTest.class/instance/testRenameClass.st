tests
testRenameClass
	| refactoring class |
	refactoring := RBRenameClassRefactoring 
				rename: (Smalltalk at: ('RBClass' , 'ToRename') asSymbol)
				to: 'RBNew' , 'ClassName' asSymbol.
	self executeRefactoring: refactoring.
	self 
		assert: ((refactoring model classNamed: ('RBNew' , 'ClassName') asSymbol) 
				parseTreeFor: #method1) = (RBParser parseMethod: 'method1
	^self method2').
	self deny: (refactoring model 
				includesClassNamed: ('RBClass' , 'ToRename') asSymbol).
	class := refactoring model 
				classNamed: ('RBSubclass' , 'OfClassToRename') asSymbol.
	self assert: class superclass 
				= (refactoring model classNamed: ('RBNew' , 'ClassName') asSymbol).
	self assert: (class parseTreeFor: #symbolReference) 
				= (RBParser parseMethod: 'symbolReference
								^#RBNewClassName').
	self assert: (class parseTreeFor: #reference) 
				= (RBParser parseMethod: 'reference
								^RBNewClassName new')
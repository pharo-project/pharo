accessing
selector: aSelector
	"Set a method's selector.  This is either the penultimate literal,
	 or, if the method has any properties or pragmas, the selector of
	 the MethodProperties stored in the penultimate literal."
	| penultimateLiteral nl | 
	Symbol internSelector:  aSelector.
	(penultimateLiteral := self penultimateLiteral) isMethodProperties
		ifTrue: [penultimateLiteral selector: aSelector]
		ifFalse: [(nl := self numLiterals) < 2 ifTrue:
					[self error: 'insufficient literals to hold selector'].
				self literalAt: nl - 1 put: aSelector]
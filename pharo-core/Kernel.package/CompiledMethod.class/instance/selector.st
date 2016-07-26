accessing
selector
	"Answer a method's selector.  This is either the penultimate literal,
	 or, if the method has any properties or pragmas, the selector of
	 the MethodProperties stored in the penultimate literal."
	| penultimateLiteral | 
	^(penultimateLiteral := self penultimateLiteral) isMethodProperties
		ifTrue: [penultimateLiteral selector]
		ifFalse: [penultimateLiteral]
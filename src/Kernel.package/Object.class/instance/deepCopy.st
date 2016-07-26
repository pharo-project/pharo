copying
deepCopy
	"Answer a copy of the receiver with its own copy of each instance variable. deepCopy does a deep copy. It should never be overridden and only be used if you want to get these very specific semantics.
It doesn't handle cycles, #veryDeepCopy does. In the future we will make it handle cycles and deprecate veryDeepCopy"


	| newObject class index |
	class := self class.
	(class == Object) ifTrue: [^self].
	class isVariable
		ifTrue: 
			[index := self basicSize.
			newObject := class basicNew: index.
			[index > 0]
				whileTrue: 
					[newObject basicAt: index put: (self basicAt: index) deepCopy.
					index := index - 1]]
		ifFalse: [newObject := class basicNew].
	index := class instSize.
	[index > 0]
		whileTrue: 
			[newObject instVarAt: index put: (self instVarAt: index) deepCopy.
			index := index - 1].
	^newObject
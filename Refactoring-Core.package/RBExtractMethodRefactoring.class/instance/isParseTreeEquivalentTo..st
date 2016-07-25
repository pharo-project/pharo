transforming
isParseTreeEquivalentTo: aSelector 
	| tree definingClass |
	definingClass := class whoDefinesMethod: aSelector.
	tree := definingClass parseTreeFor: aSelector.
	tree isNil ifTrue: [^false].
	tree isPrimitive ifTrue: [^false].
	(tree body equalTo: extractedParseTree body
		exceptForVariables: (tree arguments collect: [:each | each name])) 
			ifFalse: [^false].
	(definingClass = class or: 
			[(tree superMessages detect: 
					[:each | 
					(class superclass whichClassIncludesSelector: aSelector) 
						~= (definingClass superclass whichClassIncludesSelector: each)]
				ifNone: [nil]) isNil]) 
		ifFalse: [^false].
	^self shouldUseExistingMethod: aSelector
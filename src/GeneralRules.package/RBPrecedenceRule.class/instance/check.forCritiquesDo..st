running
check: aNode forCritiquesDo: aCriticBlock
	| leftOperand |
	
	aNode isMessage ifFalse: [ ^ self ].
	aNode selector = #* ifFalse: [ ^ self ].
	leftOperand := aNode receiver.
	leftOperand isMessage ifFalse: [ ^ self ].
	leftOperand hasParentheses ifTrue: [ ^ self ].
	(#(#+ #-) includes: leftOperand selector) ifFalse: [ ^ self ].
	
	aCriticBlock cull: (self critiqueFor: aNode)
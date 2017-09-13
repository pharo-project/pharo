running
check: aNode forCritiquesDo: aCriticBlock
	| parent |
	aNode isLiteralNode ifFalse: [ ^ self ].
	(#(true false) includes: aNode value) ifFalse: [ ^ self ].
	(parent := aNode parent) ifNil: [ ^ self ].
	parent isMessage ifFalse: [ ^ self ].
	(#(#= #== #~= #~~) includes: parent selector) ifFalse: [ ^ self ].
	
	aCriticBlock cull: (self critiqueFor: aNode)
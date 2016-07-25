*renraku
check: aMethod forCritiquesDo: aCriticBlock

	(self matcher canMatchMethod: aMethod) ifTrue: [
		| node |
		node := self matcher
			executeTree: aMethod ast
			initialAnswer: nil.
		node ifNotNil: [ 
			^ aCriticBlock cull: ( 
				self critiqueFor: node about: aMethod) ] ].
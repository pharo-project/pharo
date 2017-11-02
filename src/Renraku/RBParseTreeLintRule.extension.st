Extension { #name : #RBParseTreeLintRule }

{ #category : #'*renraku' }
RBParseTreeLintRule >> check: aMethod forCritiquesDo: aCriticBlock [

	(self matcher canMatchMethod: aMethod) ifTrue: [
		| node |
		node := self matcher
			executeTree: aMethod ast
			initialAnswer: nil.
		node ifNotNil: [ 
			^ aCriticBlock cull: ( 
				self critiqueFor: node about: aMethod) ] ].
]

{ #category : #'*renraku' }
RBParseTreeLintRule >> critiqueFor: aNode about: aMethod [
	| anchor | 

	anchor := (aNode isKindOf: RBProgramNode)
		ifFalse: [ ReSourceAnchor entity: aMethod ]
		ifTrue: [
			ReIntervalSourceAnchor
				entity: aMethod
				interval: aNode sourceInterval ].
			
	^ ReTrivialCritique
		withAnchor: anchor 
		by: self
]

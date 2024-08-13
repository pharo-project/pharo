Extension { #name : 'RBParseTreeLintRule' }

{ #category : '*Renraku' }
RBParseTreeLintRule >> check: aMethod forCritiquesDo: aCriticBlock [

	(self matcher canMatchMethod: aMethod) ifTrue: [
		| node |
		node := self matcher
			executeTree: aMethod ast
			initialAnswer: nil.
		node ifNotNil: [
			^ aCriticBlock cull: (
				self critiqueFor: node about: aMethod) ] ]
]

{ #category : '*Renraku' }
RBParseTreeLintRule >> critiqueFor: aNode about: aMethod [
	| anchor |

	anchor := (aNode isKindOf: ASTProgramNode)
		ifFalse: [ ReSourceAnchor entity: aMethod ]
		ifTrue: [
			ReIntervalSourceAnchor
				entity: aMethod
				interval: aNode sourceInterval ].

	^ ReTrivialCritique
		withAnchor: anchor
		by: self
]

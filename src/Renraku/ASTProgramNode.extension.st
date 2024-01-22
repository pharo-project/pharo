Extension { #name : 'ASTProgramNode' }

{ #category : '*Renraku' }
ASTProgramNode >> banChecksForValidation [
"returns ban levels that have to be chacked before validation"

	self methodNode ifNotNil: [ :mNode |
		mNode compiledMethod ifNotNil: [ :method |
			^ method banChecksForValidation ] ].

	^ #()
]

{ #category : '*Renraku' }
ASTProgramNode >> banLevelCandidates [
"returns code entities which can ban a rule for this entity"

	self methodNode ifNotNil: [ :mNode |
		mNode compiledMethod ifNotNil: [ :method |
			^ method banLevelCandidates ] ].

	^ #()
]

{ #category : '*Renraku' }
ASTProgramNode >> critiques [
	^ ReCriticEngine critiquesOf: self
]

{ #category : '*Renraku' }
ASTProgramNode >> match: aNode onSuccess: aBlock onFailure: anAlternativeBlock [
	| context |

	context := SmallDictionary new.

	(self match: aNode inContext: context)
		ifTrue: [ aBlock cull: context ]
		ifFalse: anAlternativeBlock
]

{ #category : '*Renraku' }
ASTProgramNode >> nodeAtTraversalIndex: anIndex [
	| index |
	index := 0.

	self nodesDo: [ :n |
		index := index + 1.
		index = anIndex ifTrue: [ ^ n ] ].

	NotFound signal
]

{ #category : '*Renraku' }
ASTProgramNode >> package [
	"a hack to make the critique check working"
	"the package information is used to determin bans"

	^ nil
]

{ #category : '*Renraku' }
ASTProgramNode >> suitableRulesFrom: aReRuleManager [
	^ aReRuleManager nodeRules
]

{ #category : '*Renraku' }
ASTProgramNode >> traversalIndexOf: aNode [
	| index |
	index := 0.

	self nodesDo: [ :n |
		index := index + 1.
		n == aNode ifTrue: [ ^ index ] ].

	^ -1
]

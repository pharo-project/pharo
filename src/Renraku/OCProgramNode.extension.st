Extension { #name : 'OCProgramNode' }

{ #category : '*Renraku' }
OCProgramNode >> banChecksForValidation [
"returns ban levels that have to be chacked before validation"

	self methodNode ifNotNil: [ :mNode |
		mNode compiledMethod ifNotNil: [ :method |
			^ method banChecksForValidation ] ].

	^ #()
]

{ #category : '*Renraku' }
OCProgramNode >> banLevelCandidates [
"returns code entities which can ban a rule for this entity"

	self methodNode ifNotNil: [ :mNode |
		mNode compiledMethod ifNotNil: [ :method |
			^ method banLevelCandidates ] ].

	^ #()
]

{ #category : '*Renraku' }
OCProgramNode >> critiques [
	^ ReCriticEngine critiquesOf: self
]

{ #category : '*Renraku' }
OCProgramNode >> match: aNode onSuccess: aBlock onFailure: anAlternativeBlock [
	| context |

	context := SmallDictionary new.

	(self match: aNode inContext: context)
		ifTrue: [ aBlock cull: context ]
		ifFalse: anAlternativeBlock
]

{ #category : '*Renraku' }
OCProgramNode >> nodeAtTraversalIndex: anIndex [
	| index |
	index := 0.

	self nodesDo: [ :n |
		index := index + 1.
		index = anIndex ifTrue: [ ^ n ] ].

	NotFound signal
]

{ #category : '*Renraku' }
OCProgramNode >> package [
	"a hack to make the critique check working"
	"the package information is used to determin bans"

	^ nil
]

{ #category : '*Renraku' }
OCProgramNode >> suitableRulesFrom: aReRuleManager [
	^ aReRuleManager nodeRules
]

{ #category : '*Renraku' }
OCProgramNode >> traversalIndexOf: aNode [
	| index |
	index := 0.

	self nodesDo: [ :n |
		index := index + 1.
		n == aNode ifTrue: [ ^ index ] ].

	^ -1
]

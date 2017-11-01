Extension { #name : #RBProgramNode }

{ #category : #'*renraku' }
RBProgramNode >> banChecksForValidation [
"returns ban levels that have to be chacked before validation"

	self methodNode ifNotNil: [ :mNode |
		mNode compiledMethod ifNotNil: [ :method |
			^ method banChecksForValidation ] ].
	
	^ #()
]

{ #category : #'*renraku' }
RBProgramNode >> banLevelCandidates [
"returns code entities which can ban a rule for this entity"

	self methodNode ifNotNil: [ :mNode |
		mNode compiledMethod ifNotNil: [ :method |
			^ method banLevelCandidates ] ].
	
	^ #()
]

{ #category : #'*renraku' }
RBProgramNode >> critiques [
	<eProperty>

	^ ReCriticEngine critiquesOf: self
]

{ #category : #'*renraku' }
RBProgramNode >> glmCritiqueSourceIn: aGLMCompositePresentation [ 
	
	^ self gtInspectorSourceCodeIn: aGLMCompositePresentation
]

{ #category : #'*renraku' }
RBProgramNode >> match: aNode onSuccess: aBlock onFailure: anAlternativeBlock [ 
	| context |
	
	context := SmallDictionary new.
	
	(self match: aNode inContext: context)
		ifTrue: [ aBlock cull: context ]
		ifFalse: anAlternativeBlock
]

{ #category : #'*renraku' }
RBProgramNode >> nodeAtTraversalIndex: anIndex [
	| index |
	index := 0.

	self nodesDo: [ :n |
		index := index + 1.
		index = anIndex ifTrue: [ ^ n ] ].
	
	NotFound signal.
]

{ #category : #'*renraku' }
RBProgramNode >> package [
	"a hack to make the critique check working"
	"the package information is used to determin bans"

	^ nil
]

{ #category : #'*renraku' }
RBProgramNode >> reverseNodesDo: aBlock [ 
	self children reverseDo: [ :each | each reverseNodesDo: aBlock ].
	aBlock value: self
]

{ #category : #'*renraku' }
RBProgramNode >> suitableRulesFrom: aReRuleManager [ 
	^ aReRuleManager nodeRules
]

{ #category : #'*renraku' }
RBProgramNode >> traversalIndexOf: aNode [
	| index |
	index := 0.

	self nodesDo: [ :n |
		index := index + 1.
		n == aNode ifTrue: [ ^ index ] ].
	
	^ -1.
]

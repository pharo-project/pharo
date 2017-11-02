Extension { #name : #RBLintRule }

{ #category : #'*renraku' }
RBLintRule >> anchorFor: anEntity [

	^ ReSourceAnchor
			entity: anEntity
]

{ #category : #'*renraku' }
RBLintRule >> check: anEntity [

	^ self check: anEntity ifNoCritiques: [ "do nothing" ]
]

{ #category : #'*renraku' }
RBLintRule >> check: anEntity forCritiquesDo: aCriticBlock [

	(self basicCheck: anEntity)
		ifTrue: [ aCriticBlock cull: (self critiqueFor: anEntity) ]
]

{ #category : #'*renraku' }
RBLintRule >> check: anEntity forCritiquesDo: aCriticBlock ifNone: alternativeBlock [
	| encounteredCritique |
	
	encounteredCritique := false.

	self check: anEntity forCritiquesDo: [ :crit |
		encounteredCritique := true.
		aCriticBlock cull: crit ].
	
	encounteredCritique ifFalse: alternativeBlock
]

{ #category : #'*renraku' }
RBLintRule >> check: anEntity ifNoCritiques: alternativeBlock [
	| critics |
	
	critics := OrderedCollection new.
	self check: anEntity forCritiquesDo: [ :critic | critics add: critic ] ifNone: alternativeBlock.
	^ critics
]

{ #category : #'*renraku' }
RBLintRule class >> checksAST [

	^ false
]

{ #category : #'*renraku' }
RBLintRule class >> checksNode [

	^ false
]

{ #category : #'*renraku' }
RBLintRule >> critiqueFor: anEntity [

	^ ReTrivialCritique
		withAnchor: (self anchorFor: anEntity)
		by: self
]

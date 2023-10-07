Extension { #name : 'RBLintRule' }

{ #category : '*Renraku' }
RBLintRule >> anchorFor: anEntity [

	^ ReSourceAnchor
			entity: anEntity
]

{ #category : '*Renraku' }
RBLintRule >> check: anEntity [

	^ self check: anEntity ifNoCritiques: [ "do nothing" ]
]

{ #category : '*Renraku' }
RBLintRule >> check: anEntity forCritiquesDo: aCriticBlock [

	(self basicCheck: anEntity)
		ifTrue: [ aCriticBlock cull: (self critiqueFor: anEntity) ]
]

{ #category : '*Renraku' }
RBLintRule >> check: anEntity forCritiquesDo: aCriticBlock ifNone: alternativeBlock [
	| encounteredCritique |

	encounteredCritique := false.

	self check: anEntity forCritiquesDo: [ :crit |
		encounteredCritique := true.
		aCriticBlock cull: crit ].

	encounteredCritique ifFalse: [ alternativeBlock value ]
]

{ #category : '*Renraku' }
RBLintRule >> check: anEntity ifNoCritiques: alternativeBlock [
	| critics |

	critics := OrderedCollection new.
	self check: anEntity forCritiquesDo: [ :critic | critics add: critic ] ifNone: alternativeBlock.
	^ critics
]

{ #category : '*Renraku' }
RBLintRule class >> checksNode [

	^ false
]

{ #category : '*Renraku' }
RBLintRule >> critiqueFor: anEntity [

	^ ReTrivialCritique
		withAnchor: (self anchorFor: anEntity)
		by: self
]

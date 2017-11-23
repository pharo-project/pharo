"
Somewhere in the override hierarchy there is a pragma that tells that this method should explicitly return a value

"
Class {
	#name : #ReReturnMethodRule,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'as yet unclassified' }
ReReturnMethodRule class >> checksMethod [
	^ true
]

{ #category : #running }
ReReturnMethodRule >> check: aMethod forCritiquesDo: aCriticBlock [
	(aMethod overriddenMethods anySatisfy: [ :method |
		method hasPragmaNamed: #shouldReturn. ])
			ifFalse: [ ^ self ].
	
	aMethod ast lastIsReturn ifTrue: [ ^ self ].
	
	aCriticBlock cull: (self critiqueFor: aMethod)
]

{ #category : #accessing }
ReReturnMethodRule >> group [

	^ 'Bugs'
]

{ #category : #accessing }
ReReturnMethodRule >> name [

	^ 'Method should explicitly return a value'
]

{ #category : #accessing }
ReReturnMethodRule >> severity [

	^ #error
]

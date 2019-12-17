"
Somewhere in the override hierarchy there is a pragma that tells that this method should explicitly return a value.

Currently (2019-12-09) there is no methods in Pharo 8 which use #shouldReturn

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
ReReturnMethodRule >> basicCheck: aMethod [
	(aMethod overriddenMethods anySatisfy: [ :method | method hasPragmaNamed: #shouldReturn ]) ifFalse: [ ^ false ].

	^ aMethod ast lastIsReturn not
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

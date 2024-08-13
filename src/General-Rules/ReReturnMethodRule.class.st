"
Somewhere in the override hierarchy there is a pragma that tells that this method should explicitly return a value.

Currently (2019-12-09) there is no methods in Pharo 8 which use #shouldReturn

"
Class {
	#name : 'ReReturnMethodRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Bugs',
	#package : 'General-Rules',
	#tag : 'Bugs'
}

{ #category : 'testing - interest' }
ReReturnMethodRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReReturnMethodRule class >> group [

	^ 'Bugs'
]

{ #category : 'accessing' }
ReReturnMethodRule class >> ruleName [
	^ 'Method should explicitly return a value'
]

{ #category : 'accessing' }
ReReturnMethodRule class >> severity [

	^ #error
]

{ #category : 'running' }
ReReturnMethodRule >> basicCheck: aMethod [
	(aMethod overriddenMethods anySatisfy: [ :method | method hasPragmaNamed: #shouldReturn ]) ifFalse: [ ^ false ].

	^ aMethod ast lastIsReturn not
]

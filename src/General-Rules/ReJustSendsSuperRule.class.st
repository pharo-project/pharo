"
This smell arises when a method just forwards the message to its superclass. This often happens due to code changes or when you simply forget that you wanted to extend the behavior of a superclass method. These methods can be removed.
"
Class {
	#name : 'ReJustSendsSuperRule',
	#superclass : 'ReAbstractRule',
	#instVars : [
		'matcher'
	],
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'testing' }
ReJustSendsSuperRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReJustSendsSuperRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'JustSendsSuperRule'
]

{ #category : 'running' }
ReJustSendsSuperRule >> basicCheck: aMethod [

	aMethod ast isPrimitive ifTrue: [ ^ false ].

	"If a method has pragmas, it is not doing nothing."
	aMethod pragmas ifNotEmpty: [ ^ false ].

	^ matcher executeMethod: aMethod ast initialAnswer: false
]

{ #category : 'accessing' }
ReJustSendsSuperRule >> group [
	^ 'Optimization'
]

{ #category : 'initialization' }
ReJustSendsSuperRule >> initialize [
	super initialize.
	matcher := RBParseTreeSearcher justSendsSuper
]

{ #category : 'accessing' }
ReJustSendsSuperRule >> name [
	^ 'Method just sends super message'
]

{ #category : 'accessing' }
ReJustSendsSuperRule >> rationale [
	^ 'Check for methods that just forward the message to its superclass.'
]

{ #category : 'accessing' }
ReJustSendsSuperRule >> severity [
	^ #information
]

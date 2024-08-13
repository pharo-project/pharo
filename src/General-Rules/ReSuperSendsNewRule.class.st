"
This rule checks for method that wrongly initialize an object twice. Contrary to other Smalltalk implementations Pharo automatically calls #initialize on object creation.
A warning is raised when the statement self new initialize is found in a method.
"
Class {
	#name : 'ReSuperSendsNewRule',
	#superclass : 'ReNodeMatchRule',
	#category : 'General-Rules-Bugs',
	#package : 'General-Rules',
	#tag : 'Bugs'
}

{ #category : 'accessing' }
ReSuperSendsNewRule class >> group [
	^ 'Bugs'
]

{ #category : 'accessing' }
ReSuperSendsNewRule class >> ruleName [
	^ 'Sends super new initialize or self new initialize'
]

{ #category : 'accessing' }
ReSuperSendsNewRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'SuperSendsNewRule'
]

{ #category : 'hooks' }
ReSuperSendsNewRule >> afterCheck: aNode mappings: mappingDict [
	^ aNode methodNode methodClass isMeta
]

{ #category : 'initialization' }
ReSuperSendsNewRule >> initialize [
	super initialize.
	self matchesAny: #(
			'super new initialize'
			'(super new: `@expr) initialize'
			'self new initialize'
			'(self new: `@expr) initialize' )
]

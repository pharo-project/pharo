"
This rule checks for method that wrongly initialize an object twice. Contrary to other Smalltalk implementations Pharo automatically calls #initiailize on object creation.
A warning is raised when the statement self new initialize is found in a method.
"
Class {
	#name : #ReSuperSendsNewRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReSuperSendsNewRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SuperSendsNewRule'
]

{ #category : #hooks }
ReSuperSendsNewRule >> afterCheck: aNode mappings: mappingDict [
	^ aNode methodNode methodClass isMeta
]

{ #category : #accessing }
ReSuperSendsNewRule >> group [
	^ 'Bugs'
]

{ #category : #initialization }
ReSuperSendsNewRule >> initialize [
	super initialize.
	self matchesAny: #(
			'super new initialize'
			'(super new: `@expr) initialize'
			'self new initialize'
			'(self new: `@expr) initialize' )
]

{ #category : #accessing }
ReSuperSendsNewRule >> name [
	^ 'Sends super new initialize or self new initialize'
]

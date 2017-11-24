"
This rule checks for method that wrongly initialize an object twice. Contrary to other Smalltalk implementations Pharo automatically calls #initiailize on object creation.
A warning is raised when the statement self new initialize is found in a method.
"
Class {
	#name : #RBSuperSendsNewRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBSuperSendsNewRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SuperSendsNewRule'
]

{ #category : #hooks }
RBSuperSendsNewRule >> afterCheck: aNode mappings: mappingDict [
	^ aNode methodNode
		ifNil: [ false ]
		ifNotNil: [ :methodNode |
			methodNode method
				ifNil: [ false ]
				ifNotNil: [ :method |
					method methodClass
						ifNil: [ false ]
						ifNotNil: #isMeta ] ]
]

{ #category : #accessing }
RBSuperSendsNewRule >> group [
	^ 'Bugs'
]

{ #category : #initialization }
RBSuperSendsNewRule >> initialize [
	super initialize.
	self matchesAny: #(
			'super new initialize'
			'(super new: `@expr) initialize'
			'self new initialize'
			'(self new: `@expr) initialize' )
]

{ #category : #accessing }
RBSuperSendsNewRule >> name [
	^ 'Sends super new initialize'
]

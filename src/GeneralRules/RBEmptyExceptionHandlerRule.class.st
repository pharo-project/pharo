"
Empty exception handler blocks hide potential bugs. The situation should be handled in a more robust way.

[  .... ]
	on: Error
	do: [  ]
	
having an empty block is a bad idea because the program silently fails.
"
Class {
	#name : #RBEmptyExceptionHandlerRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBEmptyExceptionHandlerRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'EmptyExceptionHandlerRule'
]

{ #category : #hooks }
RBEmptyExceptionHandlerRule >> afterCheck: aNode mappings: mappingDict [
	| exception class |
	exception := mappingDict at: '`exception'.
	
	exception isVariable ifFalse: [ ^ false ].
	(class := Smalltalk classNamed: exception name) ifNil: [ ^ false ].
	(class includesBehavior: Exception) ifFalse: [ ^ false ].
	(class includesBehavior: Notification) ifTrue: [ ^false ].
	
	^ true
]

{ #category : #accessing }
RBEmptyExceptionHandlerRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBEmptyExceptionHandlerRule >> initialize [
	super initialize.
	self matches:
		'`@block 
			on: `exception
			do: [ :`@err | | `@temps | ]'
		
]

{ #category : #accessing }
RBEmptyExceptionHandlerRule >> name [
	^ 'Empty exception handler'
]

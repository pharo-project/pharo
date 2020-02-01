"
Empty exception handler blocks hide potential bugs. The situation should be handled in a more robust way.

[  .... ]
	on: Error
	do: [  ]
	
having an empty block is a bad idea because the program silently fails.
"
Class {
	#name : #ReEmptyExceptionHandlerRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReEmptyExceptionHandlerRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'EmptyExceptionHandlerRule'
]

{ #category : #hooks }
ReEmptyExceptionHandlerRule >> afterCheck: aNode mappings: mappingDict [
	| exception class |
	exception := mappingDict at: '`exception'.
	
	exception isVariable ifFalse: [ ^ false ].
	(class := Smalltalk classNamed: exception name) ifNil: [ ^ false ].
	(class includesBehavior: Exception) ifFalse: [ ^ false ].
	(class includesBehavior: Notification) ifTrue: [ ^false ].
	
	^ true
]

{ #category : #accessing }
ReEmptyExceptionHandlerRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
ReEmptyExceptionHandlerRule >> initialize [
	super initialize.
	self matches:
		'`@block 
			on: `exception
			do: [ :`@err | | `@temps | ]'
		
]

{ #category : #accessing }
ReEmptyExceptionHandlerRule >> name [
	^ 'Empty exception handler'
]

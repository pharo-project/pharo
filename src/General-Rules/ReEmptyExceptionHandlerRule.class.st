"
See ReDoNotSendSuperStartUpInClassSideRule comment
"
Class {
	#name : 'ReEmptyExceptionHandlerRule',
	#superclass : 'ReNodeMatchRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'accessing' }
ReEmptyExceptionHandlerRule class >> groupSelector [

	^ #potentialBugsGroup
]

{ #category : 'accessing' }
ReEmptyExceptionHandlerRule class >> ruleName [
	^ 'Empty exception handler'
]

{ #category : 'accessing' }
ReEmptyExceptionHandlerRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'EmptyExceptionHandlerRule'
]

{ #category : 'hooks' }
ReEmptyExceptionHandlerRule >> afterCheck: aNode mappings: mappingDict [
	| exception class |
	exception := mappingDict at: '`exception'.

	exception isVariable ifFalse: [ ^ false ].
	(class := aNode methodNode methodClass environment classNamed: exception name) ifNil: [ ^ false ].
	(class includesBehavior: Exception) ifFalse: [ ^ false ].
	(class includesBehavior: Notification) ifTrue: [ ^false ].

	^ true
]

{ #category : 'initialization' }
ReEmptyExceptionHandlerRule >> initialize [
	super initialize.
	self matches:
		'`@block
			on: `exception
			do: [ :`@err | | `@temps | ]'
]

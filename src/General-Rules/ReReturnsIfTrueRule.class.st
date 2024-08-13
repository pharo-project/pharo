"
Check for methods that return the value of an ifTrue: or ifFalse: message. These statements return nil when the block is not executed.
"
Class {
	#name : 'ReReturnsIfTrueRule',
	#superclass : 'ReNodeMatchRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'accessing' }
ReReturnsIfTrueRule class >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReReturnsIfTrueRule class >> ruleName [
	^ 'Returns value of ifTrue:/ifFalse: without ifFalse:/ifTrue: block'
]

{ #category : 'accessing' }
ReReturnsIfTrueRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'ReturnsIfTrueRule'
]

{ #category : 'initialization' }
ReReturnsIfTrueRule >> initialize [
	super initialize.
	self matchesAny: #(
			'^`@condition ifTrue: [| `@temps | `@.statements]'
			'^`@condition ifFalse: [| `@temps | `@.statements]' )
]

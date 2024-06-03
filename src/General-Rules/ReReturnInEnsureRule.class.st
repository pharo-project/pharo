"
Checks for return statements within ensure: blocks that can have unintended side-effects.
"
Class {
	#name : 'ReReturnInEnsureRule',
	#superclass : 'ReNodeMatchRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'accessing' }
ReReturnInEnsureRule class >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReReturnInEnsureRule class >> ruleName [
	^ 'Contains a return in an ensure: block'
]

{ #category : 'accessing' }
ReReturnInEnsureRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'ReturnInEnsureRule'
]

{ #category : 'hooks' }
ReReturnInEnsureRule >> afterCheck: aNode mappings: mappingDict [
	^ (mappingDict at: '`@.Stmts')
		anySatisfy: #containsReturn
]

{ #category : 'initialization' }
ReReturnInEnsureRule >> initialize [
	super initialize.
	self matchesAny: #(
		'`@rcv ensure: [| `@temps | `@.Stmts]'
		'`@rcv ifCurtailed: [| `@temps | `@.Stmts]')
]

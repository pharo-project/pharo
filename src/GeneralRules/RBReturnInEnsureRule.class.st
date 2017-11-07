"
Checks for return statements within ensure: blocks that can have unintended side-effects.
"
Class {
	#name : #RBReturnInEnsureRule,
	#superclass : #ReNodeMatchRule,
	#category : 'GeneralRules-Migrated'
}

{ #category : #accessing }
RBReturnInEnsureRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ReturnInEnsureRule'
]

{ #category : #hooks }
RBReturnInEnsureRule >> afterCheck: aNode mappings: mappingDict [
	^ (mappingDict at: '`@.Stmts')
		anySatisfy: #containsReturn
]

{ #category : #accessing }
RBReturnInEnsureRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBReturnInEnsureRule >> initialize [
	super initialize.
	self matchesAny: #( 
		'`@rcv ensure: [| `@temps | `@.Stmts]'
		'`@rcv ifCurtailed: [| `@temps | `@.Stmts]')
]

{ #category : #accessing }
RBReturnInEnsureRule >> name [
	^ 'Contains a return in an ensure: block'
]

"
Checks for return statements within ensure: blocks that can have unintended side-effects.
"
Class {
	#name : #ReReturnInEnsureRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReReturnInEnsureRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ReturnInEnsureRule'
]

{ #category : #hooks }
ReReturnInEnsureRule >> afterCheck: aNode mappings: mappingDict [
	^ (mappingDict at: '`@.Stmts')
		anySatisfy: #containsReturn
]

{ #category : #accessing }
ReReturnInEnsureRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
ReReturnInEnsureRule >> initialize [
	super initialize.
	self matchesAny: #( 
		'`@rcv ensure: [| `@temps | `@.Stmts]'
		'`@rcv ifCurtailed: [| `@temps | `@.Stmts]')
]

{ #category : #accessing }
ReReturnInEnsureRule >> name [
	^ 'Contains a return in an ensure: block'
]

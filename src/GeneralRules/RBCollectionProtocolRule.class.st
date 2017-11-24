"
Checks code using the do: method instead of using the collect: or select: methods. This often occurs with new people writing code. The collect: and select: variants express the source code''s intentions better.
"
Class {
	#name : #RBCollectionProtocolRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBCollectionProtocolRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'CollectionProtocolRule'
]

{ #category : #accessing }
RBCollectionProtocolRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBCollectionProtocolRule >> initialize [
	super initialize.
	self matchesAny: #(
		'`@collection do: [:`each | | `@temps | `@.Statements1. `@object add: `@arg. `@.Statements2]'
		
		'`@collection do: [:`each | | `@temps | 
			`@.Statements1. 
			`@condition ifTrue: [| `@blockTemps | 
					`@.BlockStatements1. 
					`@object add: `each. 
					`@.BlockStatements2]. 
			`@.Statements2]'
			
		'`@collection do: [:`each | | `@temps | 
			`@.Statements1. 
			`@condition ifFalse: [| `@blockTemps | 
					`@.BlockStatements1. 
					`@object add: `each. 
					`@.BlockStatements2]. 
			`@.Statements2]' )
]

{ #category : #accessing }
RBCollectionProtocolRule >> name [
	^ 'Uses do: instead of collect: or select:''s'
]

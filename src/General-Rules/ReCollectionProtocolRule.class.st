"
Checks code using the do: method instead of using the collect: or select: methods. This often occurs with new people writing code. The collect: and select: variants express the source code''s intentions better.
"
Class {
	#name : 'ReCollectionProtocolRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReCollectionProtocolRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReCollectionProtocolRule class >> ruleName [
	^ 'Uses do: instead of collect: or select:''s'
]

{ #category : 'accessing' }
ReCollectionProtocolRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'CollectionProtocolRule'
]

{ #category : 'initialization' }
ReCollectionProtocolRule >> initialize [

	super initialize.
	self 		replace:
			'`@collection do: [:`each | | `@temps | `@.Statements. `@object add: `@arg ]'
		with:
			'`@object addAll: (`@collection collect: [:`each | | `@temps | `@.Statements. `@arg ])';
			
		replace: '`@collection do: [:`each | | `@temps |
			`@.Statements.
			`@condition ifTrue: [| `@blockTemps |
					`@.BlockStatements.
					`@object add: `@arg ] ]'
		with:
			'`@object addAll: (`@collection select: [ :`each | | `@temps | `@.Statements. `@condition ifTrue: [| `@blockTemps |
					`@.BlockStatements] ] )';
			
		replace: '`@collection do: [:`each | | `@temps |
			`@.Statements.
			`@condition ifFalse: [| `@blockTemps |
					`@.BlockStatements.
					`@object add: `@arg ] ]'
		with: '`@object addAll: (`@collection select: [ :`each | | `@temps | `@.Statements. `@condition ifFalse: [| `@blockTemps |
					`@.BlockStatements] ] )'
]

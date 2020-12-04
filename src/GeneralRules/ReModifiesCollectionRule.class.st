"
Checks for remove:''s of elements inside of collection iteration methods such as do:. These can cause the do: method to break since it will walk of the end of the collection. The common fix for this problem is to copy the collection before iterating over it.

For example turning

aCol do: [ :each |  ... aCol remove:... ]

into 

aCol copy do: [ :each |  ... aCol remove:... ]
"
Class {
	#name : #ReModifiesCollectionRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReModifiesCollectionRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ModifiesCollectionRule'
]

{ #category : #initialization }
ReModifiesCollectionRule >> afterCheck: aNode mappings: mappingDict [
	^ self
		modifiesCollection: (mappingDict at: '`@collection')
		inAnyStatement: (mappingDict at: '`@.Statements')
]

{ #category : #accessing }
ReModifiesCollectionRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
ReModifiesCollectionRule >> initialize [
	super initialize.
	self 
		matchesAny: #(
			'`@collection do: [:`each | | `@temps | `@.Statements]' 
			'`@collection collect: [:`each | | `@temps | `@.Statements]' 
			'`@collection select: [:`each | | `@temps | `@.Statements]' 
			'`@collection reject: [:`each | | `@temps | `@.Statements]' 
			'`@collection inject: `@value into: [:`sum :`each | | `@temps | `@.Statements]')
		
]

{ #category : #private }
ReModifiesCollectionRule >> modifiesCollection: aCollectionNode inAnyStatement: aStatementCollection [ 
	aStatementCollection do: [ :statement |
		statement nodesDo: [ :node |
			node = aCollectionNode ifTrue: [ ^ true ] ] ].

	^ false
]

{ #category : #accessing }
ReModifiesCollectionRule >> name [
	^ 'Modifies collection while iterating over it'
]

{ #category : #accessing }
ReModifiesCollectionRule >> rationale [
	
	^ 'Checks for remove:''s of elements inside of collection iteration methods such as do:. '
]

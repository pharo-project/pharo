"
Checks for use of to:do: when a do:, with:do: or timesRepeat: when should be used.
"
Class {
	#name : #ReToDoRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReToDoRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ToDoRule'
]

{ #category : #hooks }
ReToDoRule >> afterCheck: aNode mappings: mappingDict [
	^ self
		sendsAtWith: (mappingDict at: '`each')
		inAnyOf: (mappingDict at: '`@.Statements')
]

{ #category : #accessing }
ReToDoRule >> group [

	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
ReToDoRule >> initialize [
	super initialize.
	self matches: '1 to: ``@object size do: [:`each | | `@temps | `@.Statements]'
]

{ #category : #accessing }
ReToDoRule >> name [
	^ 'Uses to:do: instead of do:, with:do: or timesRepeat:'
]

{ #category : #hooks }
ReToDoRule >> sendsAtWith: aVariableNode inAnyOf: aStatementCollection [

	aStatementCollection do: [ :statement |
		statement nodesDo: [ :node |
			(node isMessage and: [ 
			node selector = #at: and: [ 
			node arguments first = aVariableNode ] ])
				ifTrue: [ ^ true ] ] ].
		
	^ false
]

"
Checks for use of to:do: when a do:, with:do: or timesRepeat: when should be used.
"
Class {
	#name : #RBToDoRule,
	#superclass : #RBParseTreeLintRule,
	#category : #Refactoring-Critics-ParseTreeRules
}

{ #category : #accessing }
RBToDoRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ToDoRule'
]

{ #category : #accessing }
RBToDoRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBToDoRule >> initialize [
	super initialize.
	self matcher 
		matches: '1 to: ``@object size do: [:`each | | `@temps | `@.Statements]'
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					| varName variableMatcher |
					varName := node arguments last arguments first.	"`each"
					variableMatcher := RBParseTreeSearcher new.
					variableMatcher 
						matchesTree: varName
						do: [ :nod :ans | ans and: [ nod parent isMessage and: [ nod parent selector = #at: ] ] ].
					(variableMatcher executeTree: node arguments last body initialAnswer: true) 
						ifTrue: [ node ]
						ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]
]

{ #category : #accessing }
RBToDoRule >> name [
	^ 'Uses to:do: instead of do:, with:do: or timesRepeat:'
]

"
Checks for methods that appear to be modifying a collection that is owned by another object. Such modifications can cause problems especially if other variables are modified when the collection is modified. For example, CompositePart must set the containers of all its parts when adding a new component.
"
Class {
	#name : #RBCollectionMessagesToExternalObjectRule,
	#superclass : #RBParseTreeLintRule,
	#category : #Refactoring-Critics-ParseTreeRules
}

{ #category : #accessing }
RBCollectionMessagesToExternalObjectRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'CollectionMessagesToExternalObjectRule'
]

{ #category : #accessing }
RBCollectionMessagesToExternalObjectRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBCollectionMessagesToExternalObjectRule >> initialize [
	| queries |
	super initialize.
	queries := #( add: remove: addAll: removeAll: )
		collect: [ :each | '(`@Object `@message: `@args) <1s> `@Arg' expandMacrosWith: each ].
	self matcher 
		matchesAnyOf: queries
		do: [ :node :answer |
			answer isNil 
				ifTrue: [
					((node receiver selector copyFrom: 1 to: (node receiver selector size min: 2)) ~= 'as' 
						and: [
							| receiver |
							receiver := node receiver receiver.
							receiver isVariable not 
								or: [ ((#('self' 'super') includes: receiver name) 
								or: [ Smalltalk globals includesKey: receiver name asSymbol ]) not ] ]) 
							ifTrue: [ node ]
							ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]
]

{ #category : #accessing }
RBCollectionMessagesToExternalObjectRule >> name [
	^ 'Sends add:/remove: to external collection'
]

{ #category : #accessing }
RBCollectionMessagesToExternalObjectRule >> rationale [
	^ 'Checks for methods that appear to be modifying a collection that is owned by another object.'
]

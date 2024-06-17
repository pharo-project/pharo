"
Checks for methods that appear to be modifying a collection that is owned by another object. Such modifications can cause problems especially if other variables are modified when the collection is modified. For example, CompositePart must set the containers of all its parts when adding a new component.
"
Class {
	#name : 'ReCollectionMessagesToExternalObjectRule',
	#superclass : 'ReNodeMatchRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReCollectionMessagesToExternalObjectRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReCollectionMessagesToExternalObjectRule class >> ruleName [
	^ 'Sends add:/remove: to external collection'
]

{ #category : 'accessing' }
ReCollectionMessagesToExternalObjectRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'CollectionMessagesToExternalObjectRule'
]

{ #category : 'hooks' }
ReCollectionMessagesToExternalObjectRule >> afterCheck: aNode mappings: mappingDict [
	| collectionGetter collectionOwner |
	collectionGetter := mappingDict at: '`@collectionGetter:'.
	"Check selectors that assumes unowned result"
	collectionGetter = 'copy' ifTrue: [ ^ false ].
	(collectionGetter beginsWith: 'as') ifTrue: [ ^ false ].
	collectionOwner := mappingDict at: (RBPatternVariableNode named: '`@collectionOwner').
	collectionOwner isVariable ifFalse: [ ^ true ].
	"ignore for all global vars and self and super"
	^ (aNode scope lookupVar: collectionOwner name) scope ~= Smalltalk globals
]

{ #category : 'initialization' }
ReCollectionMessagesToExternalObjectRule >> initialize [
	super initialize.
	self matchesAny: #(
		'(`@collectionOwner `@collectionGetter: `@args) add:           `@arg'
		'(`@collectionOwner `@collectionGetter: `@args) remove:     `@arg'
		'(`@collectionOwner `@collectionGetter: `@args) addAll:       `@arg'
		'(`@collectionOwner `@collectionGetter: `@args) removeAll: `@arg'
		)
]

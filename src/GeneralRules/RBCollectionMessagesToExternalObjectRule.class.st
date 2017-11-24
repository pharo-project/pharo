"
Checks for methods that appear to be modifying a collection that is owned by another object. Such modifications can cause problems especially if other variables are modified when the collection is modified. For example, CompositePart must set the containers of all its parts when adding a new component.
"
Class {
	#name : #RBCollectionMessagesToExternalObjectRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBCollectionMessagesToExternalObjectRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'CollectionMessagesToExternalObjectRule'
]

{ #category : #hooks }
RBCollectionMessagesToExternalObjectRule >> afterCheck: aNode mappings: mappingDict [
	| collectionOwner |
	((mappingDict at: '`@collectionGetter:') beginsWith: 'as') ifTrue: [ ^ false ].
	collectionOwner := mappingDict at: (RBPatternVariableNode named: '`@collectionOwner').
	 collectionOwner isVariable ifFalse: [ ^ true ].
	^ self isNotSpecialVariable: collectionOwner.
	
]

{ #category : #accessing }
RBCollectionMessagesToExternalObjectRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBCollectionMessagesToExternalObjectRule >> initialize [
	super initialize.
	self matchesAny: #(
		'(`@collectionOwner `@collectionGetter: `@args) add:           `@arg'
		'(`@collectionOwner `@collectionGetter: `@args) remove:     `@arg'
		'(`@collectionOwner `@collectionGetter: `@args) addAll:       `@arg'
		'(`@collectionOwner `@collectionGetter: `@args) removeAll: `@arg'
		)
]

{ #category : #helpers }
RBCollectionMessagesToExternalObjectRule >> isNotSpecialVariable: variableNode [

	(#('self' 'super') includes: variableNode name) ifTrue: [ ^false ]. 
	(Smalltalk globals includesKey: variableNode name asSymbol) ifTrue: [ ^ false ].
	^ true
	
]

{ #category : #accessing }
RBCollectionMessagesToExternalObjectRule >> name [
	^ 'Sends add:/remove: to external collection'
]

"
Checks for code using  ""collection at: collection size"" instead of ""collection last"".
"
Class {
	#name : #ReCollectionAtCollectionSizeRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReCollectionAtCollectionSizeRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ReCollectionAtCollectionSizeRule'
]

{ #category : #accessing }
ReCollectionAtCollectionSizeRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
ReCollectionAtCollectionSizeRule >> initialize [
	super initialize.
	self  matchesAny: #('`@collection at: `@collection size')
]

{ #category : #accessing }
ReCollectionAtCollectionSizeRule >> name [
	^ 'Uses "collection at: collection size" instead of "collection last"'
]

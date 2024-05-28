"
Checks for code using  ""collection at: collection size"" instead of ""collection last"".
"
Class {
	#name : 'ReCollectionAtCollectionSizeRule',
	#superclass : 'ReNodeMatchRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'accessing' }
ReCollectionAtCollectionSizeRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReCollectionAtCollectionSizeRule class >> ruleName [
	^ 'Uses "collection at: collection size" instead of "collection last"'
]

{ #category : 'accessing' }
ReCollectionAtCollectionSizeRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'ReCollectionAtCollectionSizeRule'
]

{ #category : 'initialization' }
ReCollectionAtCollectionSizeRule >> initialize [
	super initialize.
	self  matchesAny: #('`@collection at: `@collection size')
]

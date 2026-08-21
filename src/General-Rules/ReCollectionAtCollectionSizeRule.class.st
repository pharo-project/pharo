"
Instead of accessing to an element of a sequenceable collection using the index `1` or using the collection size, it is more readable to use `first` or `last`.
This does not change the behavior but provides a better abstraction in the code.
"
Class {
	#name : 'ReCollectionAtCollectionSizeRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReCollectionAtCollectionSizeRule class >> groupSelector [

	^ #codingIdiomViolationGroup
]

{ #category : 'accessing' }
ReCollectionAtCollectionSizeRule class >> ruleName [
	^ 'Use "collection last"/"collection first" instead of "collection at: collection size"/"collection at: 1" '
]

{ #category : 'accessing' }
ReCollectionAtCollectionSizeRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'ReCollectionAtCollectionSizeRule'
]

{ #category : 'initialization' }
ReCollectionAtCollectionSizeRule >> initialize [

	super initialize.
	self
		replace: '`@collection at: `@collection size' with: '`@collection last';
		replace: '`@collection at: 1' with: '`@collection first'
]

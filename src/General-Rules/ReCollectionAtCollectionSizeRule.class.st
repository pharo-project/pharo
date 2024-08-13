"
Checks for code using  ""collection at: collection size"" instead of ""collection last"" or using ""collection at: 1"" instead of ""collection first"".
"
Class {
	#name : 'ReCollectionAtCollectionSizeRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReCollectionAtCollectionSizeRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReCollectionAtCollectionSizeRule class >> ruleName [
	^ 'Uses "collection at: collection size"/"collection at: 1" instead of "collection last"/"collection first"'
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

"
If a package contains classes that are not tagged then this is OK. But when a tag (possibly with other categorized classes) exists in the package of a class then a class should also be categorized using a class tag.

The goal is to have no classes that are shown as ""Uncategorized"" in the system browser.
"
Class {
	#name : 'ReClassNotCategorizedRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'testing' }
ReClassNotCategorizedRule class >> checksClass [

	^ true
]

{ #category : 'accessing' }
ReClassNotCategorizedRule class >> group [
	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReClassNotCategorizedRule class >> rationale [
	^ 'Classes should be categorized with a tag if other class tags exist in the package. Use "Base" as default if you can not provide a better name.'
]

{ #category : 'accessing' }
ReClassNotCategorizedRule class >> ruleName [
	^ 'Uncategorized class - missing class tag'
]

{ #category : 'accessing' }
ReClassNotCategorizedRule class >> severity [

	^ #information
]

{ #category : 'accessing' }
ReClassNotCategorizedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'ReClassNotCategorizedRule'
]

{ #category : 'running' }
ReClassNotCategorizedRule >> basicCheck: aClass [

	aClass isMeta ifTrue: [ ^ false ].

	^ aClass packageTag isRoot and: [ aClass package tags size > 1 ]
]

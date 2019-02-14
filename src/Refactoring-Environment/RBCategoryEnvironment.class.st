"
I am a RBBrowserEnvironment on a set of category names.
I containt all entities using this category name.
I am more restricted to the exact category name compared
to a package environment.

Example, all Morph subclasses in category Morphic-Base-Menus

(RBBrowserEnvironment new forClasses: Morph withAllSubclasses) forCategories: {#'Morphic-Base-Menus'}
"
Class {
	#name : #RBCategoryEnvironment,
	#superclass : #RBBrowserEnvironmentWrapper,
	#instVars : [
		'categories'
	],
	#category : #'Refactoring-Environment'
}

{ #category : #'instance creation' }
RBCategoryEnvironment class >> categories: aCollection [
	^ self
		onEnvironment: self default
		categories: aCollection
]

{ #category : #'instance creation' }
RBCategoryEnvironment class >> category: aCategory [
	^ self categories: { aCategory }
]

{ #category : #'instance creation' }
RBCategoryEnvironment class >> onEnvironment: anEnvironment categories: aCollection [
	^(self onEnvironment: anEnvironment)
		categories: aCollection;
		yourself
]

{ #category : #adding }
RBCategoryEnvironment >> addCategory: aSymbol [
	categories add: aSymbol
]

{ #category : #initialization }
RBCategoryEnvironment >> categories: aCollection [
	categories addAll: aCollection
]

{ #category : #'accessing-classes' }
RBCategoryEnvironment >> classNames [
	^ self categories 
		inject: IdentitySet new
		into: [ :answer :each | 
			answer
				addAll: (self classNamesFor: each);
				yourself ]
]

{ #category : #private }
RBCategoryEnvironment >> defaultLabel [
	| stream |
	stream := String new writeStream.
	categories do: [:each | stream nextPutAll: each; nextPut: $ ].
	^stream contents
]

{ #category : #testing }
RBCategoryEnvironment >> definesClass: aClass [ 
	^ (super definesClass: aClass) and: [ categories includes: aClass instanceSide category ]
]

{ #category : #testing }
RBCategoryEnvironment >> includesCategory: aCategory [
	^(categories includes: aCategory)
		and: [super includesCategory: aCategory]
]

{ #category : #testing }
RBCategoryEnvironment >> includesClass: aClass [ 
	^ (super includesClass: aClass) and: [ categories includes: aClass instanceSide category ]
]

{ #category : #initialization }
RBCategoryEnvironment >> initialize [
	super initialize.
	categories := IdentitySet new
]

{ #category : #testing }
RBCategoryEnvironment >> isEmpty [
	^categories isEmpty
]

{ #category : #accessing }
RBCategoryEnvironment >> packages [
	^ self classes collect: #package
]

{ #category : #copying }
RBCategoryEnvironment >> postCopy [
	super postCopy.
	categories := categories copy
]

{ #category : #printing }
RBCategoryEnvironment >> storeOn: aStream [
	aStream nextPut: $(.
	super storeOn: aStream.
	aStream nextPutAll: ' categories: '.
	categories asArray storeOn: aStream.
	aStream nextPut: $)
]

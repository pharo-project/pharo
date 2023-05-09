"
My instances provide an organization for the classes in the system, just as a ClassOrganizer organizes the messages within a class. The only difference is the methods for fileIn/Out.
 
In order to keep track of the categories I keep a #categoryMap mapping each category to the classes inside
"
Class {
	#name : #SystemOrganizer,
	#superclass : #Object,
	#instVars : [
		'environment',
		'categoryMap'
	],
	#category : #'RPackage-Core-Base'
}

{ #category : #cleanup }
SystemOrganizer class >> cleanUp: agressive [
	"Remove empty categories when cleaning aggressively"

	agressive ifTrue: [ Smalltalk organization removeEmptyPackages ]
]

{ #category : #'instance creation' }
SystemOrganizer class >> default [
	"look in the environment that the receiver organizes for the associated organization"

	^ self environment organization
]

{ #category : #accessing }
SystemOrganizer >> addCategory: catString [
	"Add a new category named catString"

	categoryMap at: catString asSymbol ifPresent: [ ^ self ] ifAbsentPut: [ OrderedCollection new ].

	SystemAnnouncer uniqueInstance classCategoryAdded: catString
]

{ #category : #accessing }
SystemOrganizer >> categories [

	^ categoryMap keys
]

{ #category : #queries }
SystemOrganizer >> categoriesMatching: matchString [
	"Return all matching categories"

	self categories ifNil: [ ^ #(  ) ].
	^ self categories select: [ :c | matchString match: c ]
]

{ #category : #queries }
SystemOrganizer >> categoryOfBehavior: behavior [
	"Answer the category associated with the argument. This method can take a Behavior or a Behavior name as parameter."

	| behaviorName |
	behaviorName := behavior isBehavior
		                ifTrue: [ behavior name ]
		                ifFalse: [ behavior ].

	categoryMap keysAndValuesDo: [ :category :classes | (classes includes: behaviorName) ifTrue: [ ^ category ] ].

	^ nil
]

{ #category : #queries }
SystemOrganizer >> classesInCategory: category [

	^ (self listAtCategoryNamed: category) collect: [ :className | self environment at: className ]
]

{ #category : #operations }
SystemOrganizer >> classify: behaviorName under: categoryName [
	"Store the argument, element, in the category named heading."

	categoryName ifNil: [ self error: 'Category cannot be nil.' ].

	categoryMap
		at: categoryName
		ifPresent: [ :classes |
		(classes includes: behaviorName) ifTrue: [ ^ self ] ]
		ifAbsent: [ self addCategory: categoryName ].

	(self categoryOfBehavior: behaviorName) ifNotNil: [ :oldCategory |
		oldCategory = categoryName ifTrue: [ ^ self ].
		self removeBehavior: behaviorName ].

	(categoryMap at: categoryName) add: behaviorName
]

{ #category : #operations }
SystemOrganizer >> classifyAll: aCollection under: categoryName [

	aCollection do: [ :element | self classify: element under: categoryName ]
]

{ #category : #accessing }
SystemOrganizer >> environment [
	 ^ environment ifNil: [ environment := Smalltalk globals]
]

{ #category : #accessing }
SystemOrganizer >> environment: aSystemDictionary [
	 environment := aSystemDictionary
]

{ #category : #testing }
SystemOrganizer >> includesCategory: aString [
	"Tests if a category is already included."

	^ self categories
		  ifNil: [ false ]
		  ifNotNil: [ :categories | categories includes: aString ]
]

{ #category : #initialization }
SystemOrganizer >> initialize [

	super initialize.
	categoryMap := Dictionary new
]

{ #category : #testing }
SystemOrganizer >> isEmptyCategoryNamed: categoryName [

	^ categoryMap
		  at: categoryName
		  ifPresent: [ :classes | classes isEmpty ]
		  ifAbsent: [ false ]
]

{ #category : #queries }
SystemOrganizer >> listAtCategoryNamed: categoryName [
	"Answer the array of elements associated with the name, categoryName."

	^ (categoryMap at: categoryName ifAbsent: [ Array new ]) asArray
]

{ #category : #queries }
SystemOrganizer >> orderedTraitsIn: category [
	"Answer an OrderedCollection containing references to the traits in the
	category whose name is the argument, category (a string). The traits
	are ordered so they can be filed in."

	| behaviors traits |
	behaviors := (self listAtCategoryNamed: category) collect: [ :title | self environment at: title ].
	traits := behaviors select: [ :each | each isTrait ].
	traits := traits asSortedCollection: [ :t1 :t2 | (t2 traitComposition allTraits includes: t1) or: [ (t1 traitComposition allTraits includes: t2) not ] ].
	^ traits asArray
]

{ #category : #operations }
SystemOrganizer >> removeBehavior: behavior [
	"Remove the behavior from all categories. I can take a Behavior or a Behavior name as argument."

	| behaviorName |
	behaviorName := behavior isBehavior
		                ifTrue: [ behavior name ]
		                ifFalse: [ behavior ].

	categoryMap keysAndValuesDo: [ :category :classes | (classes includes: behaviorName) ifTrue: [ classes remove: behaviorName ] ]
]

{ #category : #removing }
SystemOrganizer >> removeCategoriesMatching: matchString [
	"Remove all matching categories with their classes"

	(self categoriesMatching: matchString) do: [ :c | self removeSystemCategory: c ]
]

{ #category : #accessing }
SystemOrganizer >> removeCategory: category [
	"Remove the category named, cat. Create an error notificiation if the
	category has any elements in it."

	categoryMap
		at: category
		ifPresent: [ :classes | classes ifNotEmpty: [ ^ self error: 'Cannot remove non-empty category ' , category , '. Present classes: ' , classes printString ] ]
		ifAbsent: [ ^ self ].

	categoryMap removeKey: category.

	SystemAnnouncer uniqueInstance classCategoryRemoved: category
]

{ #category : #removing }
SystemOrganizer >> removeEmptyPackages [
	"Remove empty packages."

	(categoryMap select: [ :classes | classes isEmpty ]) keys
		ifNotEmpty: [ :emptyPackages |
			emptyPackages do: [ :emptyPackage | categoryMap removeKey: emptyPackage ] ]
]

{ #category : #removing }
SystemOrganizer >> removeSystemCategory: category [
	"remove all the classes and traits associated with the category"

	(self orderedTraitsIn: category) , (self superclassOrder: category) reverseDo: [ :each | each removeFromSystem ].

	self removeCategory: category
]

{ #category : #accessing }
SystemOrganizer >> renameCategory: oldCatString toBe: newCatString [
	"Rename a category. No action if new name already exists, or if old name does not exist."

	categoryMap at: newCatString ifPresent: [ "new name exists, so no action" ^ self ].

	categoryMap
		at: oldCatString
		ifPresent: [ :classes |
			categoryMap at: newCatString put: classes.
			categoryMap removeKey: oldCatString ]
		ifAbsent: [ "old name not found, so no action" ^ self ].

	SystemAnnouncer uniqueInstance classCategoryRenamedFrom: oldCatString to: newCatString
]

{ #category : #queries }
SystemOrganizer >> superclassOrder: category [
	"Answer an OrderedCollection containing references to the classes in the
	category whose name is the argument, category (a string). The classes
	are ordered with superclasses first so they can be filed in."

	| behaviors classes |
	behaviors := (self listAtCategoryNamed: category) collect: [ :title | self environment at: title ].
	classes := behaviors select: [ :each | each isBehavior ].
	^ Class superclassOrder: classes
]

"
My instances provide an organization for the classes in the system, just as a ClassOrganizer organizes the messages within a class. The only difference is the methods for fileIn/Out.
"
Class {
	#name : #SystemOrganizer,
	#superclass : #Categorizer,
	#instVars : [
		'environment'
	],
	#category : #'System-Support-Image'
}

{ #category : #cleanup }
SystemOrganizer class >> cleanUp: agressive [
	"Remove empty categories when cleaning aggressively"

	agressive ifTrue: [SystemOrganization removeEmptyCategories]
]

{ #category : #'instance creation' }
SystemOrganizer class >> default [
	"look in the environment that the receiver organizes for the associated organization"

	^ self environment organization
]

{ #category : #accessing }
SystemOrganizer >> addCategory: newCategory [
	(self includesCategory: newCategory) ifTrue: [ ^ self ].

	super addCategory: newCategory.

	SystemAnnouncer uniqueInstance classCategoryAdded: newCategory
]

{ #category : #queries }
SystemOrganizer >> categoriesMatching: matchString [
	"Return all matching categories"

	self categories ifNil: [ ^#() ].
	^ self categories select: [:c | matchString match: c]
]

{ #category : #queries }
SystemOrganizer >> classesInCategory: category [
	^ (self listAtCategoryNamed: category) collect: [ :className | self environment at: className ]
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
	categoryArray := Array new.
	categoryStops := Array new.
	elementArray := Array new
]

{ #category : #queries }
SystemOrganizer >> orderedTraitsIn: category [
	"Answer an OrderedCollection containing references to the traits in the
	category whose name is the argument, category (a string). The traits
	are ordered so they can be filed in."

	| behaviors traits |
	behaviors := (self listAtCategoryNamed: category asSymbol) collect: [ :title | self environment at: title ].
	traits := behaviors select: [ :each | each isTrait ].
	traits := traits
		asSortedCollection: [ :t1 :t2 | (t2 traitComposition allTraits includes: t1) or: [ (t1 traitComposition allTraits includes: t2) not ] ].
	^ traits asArray
]

{ #category : #removing }
SystemOrganizer >> removeCategoriesMatching: matchString [
	"Remove all matching categories with their classes"
	(self categoriesMatching: matchString) do:
		[:c | self removeSystemCategory: c]
]

{ #category : #accessing }
SystemOrganizer >> removeCategory: category [
	| r |
	r := super removeCategory: category.

	"If the category we try to remove is the default one, due to cleansing, we don't have to announce it since it is not a valid category for a Class"
	category = Default ifFalse: [
		SystemAnnouncer uniqueInstance classCategoryRemoved: category.
	].

	^ r
]

{ #category : #removing }
SystemOrganizer >> removeSystemCategory: category [
	"remove all the classes and traits associated with the category"

	 (self orderedTraitsIn: category) , (self superclassOrder: category)
		reverseDo: [:each | each removeFromSystem].

	self removeCategory: category
]

{ #category : #accessing }
SystemOrganizer >> renameCategory: oldCatString toBe: newCatString [
	| r |
	r := super renameCategory: oldCatString toBe: newCatString.
	SystemAnnouncer uniqueInstance
		classCategoryRenamedFrom: oldCatString to: newCatString.
	^ r
]

{ #category : #queries }
SystemOrganizer >> superclassOrder: category [
	"Answer an OrderedCollection containing references to the classes in the
	category whose name is the argument, category (a string). The classes
	are ordered with superclasses first so they can be filed in."

	| behaviors classes |
	behaviors := (self listAtCategoryNamed: category asSymbol) collect: [ :title | self environment at: title ].
	classes := behaviors select: [ :each | each isBehavior ].
	^ Class superclassOrder: classes
]

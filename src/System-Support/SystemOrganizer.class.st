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
	#category : #'System-Support-Image'
}

{ #category : #cleanup }
SystemOrganizer class >> cleanUp: agressive [
	"Remove empty categories when cleaning aggressively"

	agressive ifTrue: [ SystemOrganization removeEmptyPackages ]
]

{ #category : #'instance creation' }
SystemOrganizer class >> default [
	"look in the environment that the receiver organizes for the associated organization"

	^ self environment organization
]

{ #category : #accessing }
SystemOrganizer >> addCategory: packageName [

	categoryMap at: packageName asSymbol ifPresent: [ ^ self ] ifAbsentPut: [ OrderedCollection new ].

	SystemAnnouncer uniqueInstance classCategoryAdded: packageName
]

{ #category : #private }
SystemOrganizer >> basicRemoveElement: element [
	"Remove the element from all packages."

	categoryMap keysAndValuesDo: [ :category :classes | (classes includes: element) ifTrue: [ classes remove: element ] ]
]

{ #category : #accessing }
SystemOrganizer >> categories [

	^ categoryMap keys
]

{ #category : #queries }
SystemOrganizer >> categoriesMatching: matchString [
	"Return all matching categories"

	self categories ifNil: [ ^ #(  ) ].
	^ self categories select: [ :packageName | matchString match: packageName ]
]

{ #category : #queries }
SystemOrganizer >> categoryOfElement: element [
	"Answer the category associated with the argument, element."

	categoryMap keysAndValuesDo: [ :packageName :classes | (classes includes: element) ifTrue: [ ^ packageName ] ].

	^ nil
]

{ #category : #queries }
SystemOrganizer >> classesInCategory: packageName [

	^ (self listAtCategoryNamed: packageName) collect: [ :className | self environment at: className ]
]

{ #category : #operations }
SystemOrganizer >> classify: element under: packageName [

	packageName ifNil: [ self error: 'Category cannot be nil.' ].

	categoryMap
		at: packageName
		ifPresent: [ :classes | (classes includes: element) ifTrue: [ ^ self ] ]
		ifAbsent: [ self addCategory: packageName ].

	(self categoryOfElement: element) ifNotNil: [ :oldCategory |
		oldCategory = packageName ifTrue: [ ^ self ].
		self basicRemoveElement: element ].

	(categoryMap at: packageName) add: element
]

{ #category : #operations }
SystemOrganizer >> classifyAll: aCollection under: packageName [

	aCollection do: [ :element | self classify: element under: packageName ]
]

{ #category : #accessing }
SystemOrganizer >> environment [

	^ environment ifNil: [ environment := Smalltalk globals ]
]

{ #category : #accessing }
SystemOrganizer >> environment: aSystemDictionary [
	 environment := aSystemDictionary
]

{ #category : #testing }
SystemOrganizer >> includesCategory: packageName [

	^ self categories includes: packageName
]

{ #category : #initialization }
SystemOrganizer >> initialize [

	super initialize.
	categoryMap := Dictionary new
]

{ #category : #testing }
SystemOrganizer >> isEmptyCategoryNamed: packageName [

	^ categoryMap
		  at: packageName
		  ifPresent: [ :classes | classes isEmpty ]
		  ifAbsent: [ false ]
]

{ #category : #queries }
SystemOrganizer >> listAtCategoryNamed: packageName [
	"Answer classes names into packageName."

	^ (categoryMap at: packageName ifAbsent: [ Array new ]) asArray
]

{ #category : #queries }
SystemOrganizer >> orderedTraitsIn: packageName [
	"Answer an OrderedCollection containing references to the traits in the
	category whose name is the argument, category (a string). The traits
	are ordered so they can be filed in."

	| behaviors traits |
	behaviors := (self listAtCategoryNamed: packageName) collect: [ :title | self environment at: title ].
	traits := behaviors select: [ :each | each isTrait ].
	traits := traits asSortedCollection: [ :t1 :t2 | (t2 traitComposition allTraits includes: t1) or: [ (t1 traitComposition allTraits includes: t2) not ] ].
	^ traits asArray
]

{ #category : #removing }
SystemOrganizer >> removeCategoriesMatching: matchString [
	"Remove all matching package names with their classes"

	(self categoriesMatching: matchString) do: [ :c | self removeSystemCategory: c ]
]

{ #category : #accessing }
SystemOrganizer >> removeCategory: packageName [
	"Remove packageName. Create an error notificiation if the package has any elements in it."

	categoryMap
		at: packageName
		ifPresent: [ :classes | classes ifNotEmpty: [ ^ self error: 'cannot remove non-empty category ' , packageName ] ]
		ifAbsent: [ ^ self ].

	categoryMap removeKey: packageName.

	SystemAnnouncer uniqueInstance classCategoryRemoved: packageName
]

{ #category : #operations }
SystemOrganizer >> removeElement: element [
	^ self basicRemoveElement: element
]

{ #category : #removing }
SystemOrganizer >> removeEmptyPackages [
	"Remove empty packages."

	(categoryMap select: [ :classes | classes isEmpty ]) keys
		ifNotEmpty: [ :emptyPackages |
			emptyPackages do: [ :emptyPackage | categoryMap removeKey: emptyPackage ] ]
]

{ #category : #removing }
SystemOrganizer >> removeSystemCategory: packageName [
	"remove all the classes and traits associated with the packageName"

	(self orderedTraitsIn: packageName) , (self superclassOrder: packageName) reverseDo: [ :each | each removeFromSystem ].

	self removeCategory: packageName
]

{ #category : #accessing }
SystemOrganizer >> renameCategory: oldPackageName toBe: newPackageName [
	"Rename a category. No action if new name already exists, or if old name does not exist."

	categoryMap at: newPackageName ifPresent: [ "new name exists, so no action" ^ self ].

	categoryMap
		at: oldPackageName
		ifPresent: [ :classes |
			categoryMap at: newPackageName put: classes.
			categoryMap removeKey: oldPackageName ]
		ifAbsent: [ "old name not found, so no action" ^ self ].

	SystemAnnouncer uniqueInstance classCategoryRenamedFrom: oldPackageName to: newPackageName
]

{ #category : #queries }
SystemOrganizer >> superclassOrder: packageName [
	"Answer an OrderedCollection containing references to the classes in the
	category whose name is the argument, category (a string). The classes
	are ordered with superclasses first so they can be filed in."

	| behaviors classes |
	behaviors := (self listAtCategoryNamed: packageName) collect: [ :title | self environment at: title ].
	classes := behaviors select: [ :each | each isBehavior ].
	^ Class superclassOrder: classes
]

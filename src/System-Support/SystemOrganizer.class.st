"
My instances provide an organization for the classes in the system, just as a ClassOrganizer organizes the messages within a class. The only difference is the methods for fileIn/Out.
 
In order to keep track of the categories I keep a #categoryMap mapping each category to the classes inside
"
Class {
	#name : #SystemOrganizer,
	#superclass : #Object,
	#instVars : [
		'environment',
		'packagesMap'
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
SystemOrganizer >> addPackageNamed: packageName [

	packagesMap at: packageName asSymbol ifPresent: [ ^ self ] ifAbsentPut: [ OrderedCollection new ].

	SystemAnnouncer uniqueInstance classCategoryAdded: packageName
]

{ #category : #queries }
SystemOrganizer >> classesInPackageNamed: packageName [

	^ (self listAtCategoryNamed: packageName) collect: [ :className | self environment at: className ]
]

{ #category : #operations }
SystemOrganizer >> classify: element under: packageName [

	packageName ifNil: [ self error: 'Category cannot be nil.' ].

	packagesMap
		at: packageName
		ifPresent: [ :classes | (classes includes: element) ifTrue: [ ^ self ] ]
		ifAbsent: [ self addPackageNamed: packageName ].
	(self packageNameOfElement: element) ifNotNil: [ :oldCategory |
		oldCategory = packageName ifTrue: [ ^ self ].
		self removeElement: element ].

	(packagesMap at: packageName) add: element
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

	^ self packageNames includes: packageName
]

{ #category : #initialization }
SystemOrganizer >> initialize [

	super initialize.
	packagesMap := Dictionary new
]

{ #category : #testing }
SystemOrganizer >> isEmptyCategoryNamed: packageName [

	^ packagesMap
		  at: packageName
		  ifPresent: [ :classes | classes isEmpty ]
		  ifAbsent: [ false ]
]

{ #category : #queries }
SystemOrganizer >> listAtCategoryNamed: packageName [
	"Answer classes names into packageName."

	^ (packagesMap at: packageName ifAbsent: [ Array new ]) asArray
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

{ #category : #queries }
SystemOrganizer >> packageNameOfElement: element [
	"Answer the category associated with the argument, element."

	packagesMap keysAndValuesDo: [ :packageName :classes | (classes includes: element) ifTrue: [ ^ packageName ] ].

	^ nil
]

{ #category : #accessing }
SystemOrganizer >> packageNames [

	^ packagesMap keys
]

{ #category : #queries }
SystemOrganizer >> packageNamesMatching: matchString [
	"Return all matching package names"

	^ self packageNames select: [ :packageName | matchString match: packageName ]
]

{ #category : #removing }
SystemOrganizer >> removeCategoriesMatching: matchString [
	"Remove all matching package names with their classes"

	(self packageNamesMatching: matchString) do: [ :c | self removeSystemCategory: c ]
]

{ #category : #accessing }
SystemOrganizer >> removeCategory: packageName [
	"Remove packageName. Create an error notificiation if the package has any elements in it."

	packagesMap
		at: packageName
		ifPresent: [ :classes | classes ifNotEmpty: [ ^ self error: 'cannot remove non-empty category ' , packageName ] ]
		ifAbsent: [ ^ self ].

	packagesMap removeKey: packageName.

	SystemAnnouncer uniqueInstance classCategoryRemoved: packageName
]

{ #category : #operations }
SystemOrganizer >> removeElement: element [
	"Remove the element from all packages."

	packagesMap keysAndValuesDo: [ :category :classes | (classes includes: element) ifTrue: [ classes remove: element ] ]
]

{ #category : #removing }
SystemOrganizer >> removeEmptyPackages [
	"Remove empty packages."

	(packagesMap select: [ :classes | classes isEmpty ]) keys
		ifNotEmpty: [ :emptyPackages |
			emptyPackages do: [ :emptyPackage | packagesMap removeKey: emptyPackage ] ]
]

{ #category : #removing }
SystemOrganizer >> removeSystemCategory: packageName [
	"remove all the classes and traits associated with the packageName"

	(self orderedTraitsIn: packageName) , (self superclassOrder: packageName) reverseDo: [ :each | each removeFromSystem ].

	self removeCategory: packageName
]

{ #category : #accessing }
SystemOrganizer >> renamePackageNamed: oldPackageName to: newPackageName [
	"Rename a category. No action if new name already exists, or if old name does not exist."

	packagesMap at: newPackageName ifPresent: [ "new name exists, so no action" ^ self ].

	packagesMap
		at: oldPackageName
		ifPresent: [ :classes |
			packagesMap at: newPackageName put: classes.
			packagesMap removeKey: oldPackageName ]
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

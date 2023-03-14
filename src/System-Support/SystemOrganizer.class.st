"
My instances provide an organization for the classes in the system, just as a ClassOrganizer organizes the messages within a class. The only difference is the methods for fileIn/Out.
 
Instances consist of an Array of category names (categoryArray), each of which refers to an Array of elements (elementArray). This association is made through an Array of stop indices (categoryStops), each of which is 
the index in elementArray of the last element (if any) of the corresponding category. For example: categories := Array with: 'firstCat' with: 'secondCat' with: 'thirdCat'. stops := Array with: 1 with: 4 with: 4. 
elements := Array with: #a with: #b with: #c with: #d. This means that category firstCat has only #a, secondCat has #b, #c, and #d, and thirdCat has no elements. This means that stops at: stops size must be the same as elements size.
 Instance Variables
      categoryArray:          <SequenceableCollection of: Object>
      categoryStops:          <SequenceableCollection of: Integer>
      elementArray:           <SequenceableCollection of: Object>
categoryArray
       - holds the list of categories.
       A category could be any Object but is generally a String or Symbol.
       Categories should be unique (categoryArray asSet size = categoryArray size)
categoryStops
       - holds the index of last element belonging to each category.
       There should be a category stop for each category (categoryStops size = categoryArray size).
       The categoryStops should be sorted (categoryStops sorted = categoryStops).
       A category stop equal to its predecessor (= 0 for the first category stop) denotes an empty category.
elementArray
      - holds the elements to be classified. The elements are sorted by category.
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

	agressive ifTrue: [SystemOrganization removeEmptyCategories]
]

{ #category : #'instance creation' }
SystemOrganizer class >> default [
	"look in the environment that the receiver organizes for the associated organization"

	^ self environment organization
]

{ #category : #accessing }
SystemOrganizer >> addCategory: catString [
	"Add a new category named catString"

	categoryMap at: catString ifPresent: [ ^ self ] ifAbsentPut: [ OrderedCollection new ].

	SystemAnnouncer uniqueInstance classCategoryAdded: catString
]

{ #category : #private }
SystemOrganizer >> basicRemoveElement: element [
	"Remove the element from all categories."

	| impactedCategories |
	impactedCategories := OrderedCollection new.
	categoryMap keysAndValuesDo: [ :category :classes |
		(classes includes: element) ifTrue: [
			classes remove: element.
			impactedCategories add: category ] ].

	impactedCategories do: [ :category | (categoryMap at: category) ifEmpty: [ self removeCategory: category ] ]
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
SystemOrganizer >> categoryOfElement: element [
	"Answer the category associated with the argument, element."

	categoryMap keysAndValuesDo: [ :category :classes | (classes includes: element) ifTrue: [ ^ category ] ].

	^ nil
]

{ #category : #queries }
SystemOrganizer >> classesInCategory: category [

	^ (self listAtCategoryNamed: category) collect: [ :className | self environment at: className ]
]

{ #category : #operations }
SystemOrganizer >> classify: element under: categoryName [
	"Store the argument, element, in the category named heading."

	| catName catIndex elemIndex realHeading |
	categoryName ifNil: [ self error: 'Category cannot be nil.' ].
	realHeading := categoryName asSymbol.

	categoryMap
		at: realHeading
		ifPresent: [ :classes | (classes includes: element) ifTrue: [ ^ self ] ]
		ifAbsent: [ self addCategory: realHeading ].

	(catName := self categoryOfElement: element) = realHeading ifTrue: [ ^ self ]. "done if already under that category"

	catName ifNotNil: [ self basicRemoveElement: element ]. "remove if in another category"

	(categoryMap at: realHeading) add: element
]

{ #category : #operations }
SystemOrganizer >> classifyAll: aCollection under: heading [

	aCollection do: [ :element | self classify: element under: heading ]
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
	behaviors := (self listAtCategoryNamed: category asSymbol) collect: [ :title | self environment at: title ].
	traits := behaviors select: [ :each | each isTrait ].
	traits := traits
		asSortedCollection: [ :t1 :t2 | (t2 traitComposition allTraits includes: t1) or: [ (t1 traitComposition allTraits includes: t2) not ] ].
	^ traits asArray
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
		ifPresent: [ :classes | classes ifNotEmpty: [ ^ self error: 'cannot remove non-empty category ' , category ] ]
		ifAbsent: [ ^ self ].

	categoryMap removeKey: category.

	SystemAnnouncer uniqueInstance classCategoryRemoved: category
]

{ #category : #operations }
SystemOrganizer >> removeElement: element [
	^ self basicRemoveElement: element
]

{ #category : #operations }
SystemOrganizer >> removeEmptyCategories [
	"Remove empty categories."

	(categoryMap select: [ :classes | classes isEmpty ]) keys do: [ :category | self removeCategory: category ]
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
	behaviors := (self listAtCategoryNamed: category asSymbol) collect: [ :title | self environment at: title ].
	classes := behaviors select: [ :each | each isBehavior ].
	^ Class superclassOrder: classes
]

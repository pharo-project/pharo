"
My instances provide an organization for the classes in the system, just as a ClassOrganizer organizes the messages within a class. The only difference is the methods for fileIn/Out.
"
Class {
	#name : #SystemOrganizer,
	#superclass : #Object,
	#instVars : [
		'categoryArray',
		'categoryStops',
		'elementArray',
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
SystemOrganizer >> addCategory: catString [
	"Add a new category named heading.
	If default category exists and is empty, remove it.
	If nextCategory is nil, then add the new one at the end,
	otherwise, insert it before nextCategory."

	| index newCategory |
	newCategory := catString asSymbol.
	(categoryArray indexOf: newCategory) > 0 ifTrue: [ ^ self ]. "heading already exists, so done"

	index := categoryArray size + 1.
	categoryArray := categoryArray copyReplaceFrom: index to: index - 1 with: (Array with: newCategory).
	categoryStops := categoryStops copyReplaceFrom: index to: index - 1 with: (Array with: (index = 1
				                   ifTrue: [ 0 ]
				                   ifFalse: [ categoryStops at: index - 1 ])).

	SystemAnnouncer uniqueInstance classCategoryAdded: catString
]

{ #category : #private }
SystemOrganizer >> assertInvariant [

	[ elementArray size = categoryStops last ] assert
]

{ #category : #private }
SystemOrganizer >> basicRemoveElement: element [
	"Remove the selector, element, from all categories."
	| categoryIndex elementIndex nextStop newElements |
	categoryIndex := 1.
	elementIndex := 0.
	nextStop := 0.
	"nextStop keeps track of the stops in the new element array"
	newElements := (Array new: elementArray size) writeStream.
	[(elementIndex := elementIndex + 1) <= elementArray size]
		whileTrue:
			[[elementIndex > (categoryStops at: categoryIndex)]
				whileTrue:
					[categoryStops at: categoryIndex put: nextStop.
					categoryIndex := categoryIndex + 1].
			(elementArray at: elementIndex) = element
				ifFalse:
					[nextStop := nextStop + 1.
					newElements nextPut: (elementArray at: elementIndex)]].
	[categoryIndex <= categoryStops size]
		whileTrue:
			[categoryStops at: categoryIndex put: nextStop.
			categoryIndex := categoryIndex + 1].
	elementArray := newElements contents.
	self assertInvariant
]

{ #category : #accessing }
SystemOrganizer >> categories [

	^ categoryArray
]

{ #category : #accessing }
SystemOrganizer >> categories: anArray [
	"Reorder my categories to be in order of the argument, anArray. If the
	resulting organization does not include all elements, then give an error."

	| newCategories newStops newElements newElementsSet catName list runningTotal |

	anArray size < 2 ifTrue: [ ^ self ].

	newCategories := Array new: anArray size.
	newStops := Array new: anArray size.
	newElements := OrderedCollection new: anArray size.
	runningTotal := 0.
	1 to: anArray size do:
		[:i |
		catName := (anArray at: i) asSymbol.
		list := self listAtCategoryNamed: catName.
		newElements addAllLast: list.
		newCategories at: i put: catName.
		newStops at: i put: (runningTotal := runningTotal + list size)].
	newElements := newElements asArray.
	"create a temporary set for super-fast includes check"
	newElementsSet := newElements asSet.
	elementArray do:
		[:element | "check to be sure all elements are included"
		(newElementsSet includes: element)
			ifFalse: [^self error: 'New categories must match old ones']].
	"Everything is good, now update my three arrays."
	categoryArray := newCategories.
	categoryStops := newStops.
	elementArray := newElements
]

{ #category : #queries }
SystemOrganizer >> categoriesMatching: matchString [
	"Return all matching categories"

	self categories ifNil: [ ^#() ].
	^ self categories select: [:c | matchString match: c]
]

{ #category : #queries }
SystemOrganizer >> categoryOfElement: element [
	"Answer the category associated with the argument, element."

	| index |
	index := self numberOfCategoryOfElement: element.
	^ index = 0
		ifTrue: [ nil]
		ifFalse: [ categoryArray at: index ]
]

{ #category : #queries }
SystemOrganizer >> classesInCategory: category [
	^ (self listAtCategoryNamed: category) collect: [ :className | self environment at: className ]
]

{ #category : #operations }
SystemOrganizer >> classify: element under: heading [
	"Store the argument, element, in the category named heading."

	| catName catIndex elemIndex realHeading |
	heading ifNil: [ self error: 'Category cannot be nil.' ].
	realHeading := heading asSymbol.

	(catName := self categoryOfElement: element) = realHeading ifTrue: [ ^ self ]. "done if already under that category"

	catName ifNotNil: [ self basicRemoveElement: element ]. "remove if in another category"

	(categoryArray indexOf: realHeading) = 0 ifTrue: [ self addCategory: realHeading ].

	catIndex := categoryArray indexOf: realHeading.
	elemIndex := catIndex > 1
		             ifTrue: [ categoryStops at: catIndex - 1 ]
		             ifFalse: [ 0 ].
	[ (elemIndex := elemIndex + 1) <= (categoryStops at: catIndex) and: [ element >= (elementArray at: elemIndex) ] ] whileTrue.

	"elemIndex is now the index for inserting the element. Do the insertion before it."
	elementArray := elementArray copyReplaceFrom: elemIndex to: elemIndex - 1 with: (Array with: element).

	"add one to stops for this and later categories"
	catIndex to: categoryArray size do: [ :i | categoryStops at: i put: (categoryStops at: i) + 1 ].

	self assertInvariant
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

{ #category : #private }
SystemOrganizer >> firstIndexOfCategoryNumber: anInteger [

	anInteger < 1 ifTrue: [ ^ nil ].
	^ anInteger > 1
		  ifTrue: [ (categoryStops at: anInteger - 1) + 1 ]
		  ifFalse: [ 1 ]
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

{ #category : #testing }
SystemOrganizer >> isEmptyCategoryNamed: categoryName [

	| index firstIndex lastIndex |
	index := categoryArray indexOf: categoryName ifAbsent: [ ^ false ].

	(index < 1 or: [ index > categoryStops size ]) ifTrue: [ ^ true ].
	firstIndex := self firstIndexOfCategoryNumber: index.
	lastIndex := self lastIndexOfCategoryNumber: index.
	^ firstIndex > lastIndex
]

{ #category : #private }
SystemOrganizer >> lastIndexOfCategoryNumber: anInteger [

	anInteger > categoryStops size ifTrue: [ ^ nil ].
	^ categoryStops at: anInteger
]

{ #category : #queries }
SystemOrganizer >> listAtCategoryNamed: categoryName [
	"Answer the array of elements associated with the name, categoryName."

	| index firstIndex lastIndex |
	index := categoryArray indexOf: categoryName ifAbsent: [ ^ Array new ].

	(index < 1 or: [ index > categoryStops size ]) ifTrue: [ ^ nil ].
	firstIndex := self firstIndexOfCategoryNumber: index.
	lastIndex := self lastIndexOfCategoryNumber: index.
	^ elementArray copyFrom: firstIndex to: lastIndex
]

{ #category : #private }
SystemOrganizer >> numberOfCategoryOfElement: element [
	"Answer the index of the category with which the argument, element, is
	associated."

	| indexOfElementInElementArray |
	indexOfElementInElementArray := elementArray identityIndexOf: element asSymbol ifAbsent: [ ^ 0 ].
	^ categoryStops findFirst: [ :stopIndex | stopIndex >= indexOfElementInElementArray ]
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
SystemOrganizer >> removeCategory: cat [
	"Remove the category named, cat. Create an error notificiation if the
	category has any elements in it."

	| index lastStop |
	index := categoryArray indexOf: cat ifAbsent: [ ^ self ].
	lastStop := index = 1
		            ifTrue: [ 0 ]
		            ifFalse: [ categoryStops at: index - 1 ].
	(categoryStops at: index) - lastStop > 0 ifTrue: [ ^ self error: 'cannot remove non-empty category ' , cat ].
	categoryArray := categoryArray copyReplaceFrom: index to: index with: Array new.
	categoryStops := categoryStops copyReplaceFrom: index to: index with: Array new.
	categoryArray isEmpty ifTrue: [
		categoryArray := Array new.
		categoryStops := Array with: 0 ].

	SystemAnnouncer uniqueInstance classCategoryRemoved: cat
]

{ #category : #operations }
SystemOrganizer >> removeElement: element [
	^ self basicRemoveElement: element
]

{ #category : #operations }
SystemOrganizer >> removeEmptyCategories [
	"Remove empty categories."

	| categoryIndex currentStop keptCategories keptStops |
	keptCategories := (Array new: 16) writeStream.
	keptStops := (Array new: 16) writeStream.
	currentStop := categoryIndex := 0.
	[ (categoryIndex := categoryIndex + 1) <= categoryArray size ] whileTrue: [
		(categoryStops at: categoryIndex) > currentStop ifTrue: [
			keptCategories nextPut: (categoryArray at: categoryIndex).
			keptStops nextPut: (currentStop := categoryStops at: categoryIndex) ] ].
	categoryArray := keptCategories contents.
	categoryStops := keptStops contents.
	categoryArray isEmpty ifTrue: [ "ClassOrganizer allInstancesDo: [:co | co removeEmptyCategories]."
		categoryArray := Array new.
		categoryStops := Array with: 0 ]
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
	"Rename a category. No action if new name already exists, or if old name does not exist."

	| index newCategory |
	newCategory := newCatString asSymbol.
	(categoryArray indexOf: newCategory) > 0 ifTrue: [ ^ self ]. "new name exists, so no action"
	(index := categoryArray indexOf: oldCatString asSymbol) = 0 ifTrue: [ ^ self ]. "old name not found, so no action"
	categoryArray := categoryArray copy. "need to change identity so smart list update will notice the change"
	categoryArray at: index put: newCategory.

	SystemAnnouncer uniqueInstance classCategoryRenamedFrom: oldCatString to: newCatString
]

{ #category : #operations }
SystemOrganizer >> sortCategories [

	| privateCategories publicCategories newCategories |
	privateCategories := self categories select: [ :one | (one findString: 'private' startingAt: 1 caseSensitive: false) = 1 ].
	publicCategories := self categories copyWithoutAll: privateCategories.
	newCategories := publicCategories asSortedCollection asOrderedCollection
		                 addAll: privateCategories;
		                 asArray.
	self categories: newCategories
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

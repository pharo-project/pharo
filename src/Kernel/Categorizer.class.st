"
A Categorizer is responsible to manage the class categories and method protocols. 

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

Class variables
       Default is the default category used to classify yet unclassified methods of a class
       NullCategory is the category to be displayed in a Browser for a class having no method.
"
Class {
	#name : #Categorizer,
	#superclass : #Object,
	#instVars : [
		'categoryArray',
		'categoryStops',
		'elementArray'
	],
	#classVars : [
		'Default',
		'NullCategory'
	],
	#category : #'Kernel-Classes'
}

{ #category : #'class initialization' }
Categorizer class >> allCategory [
	"Return a symbol that represents the virtual all methods category."

	^#'-- all --'
]

{ #category : #'class initialization' }
Categorizer class >> default [ 
	^ Default
]

{ #category : #'instance creation' }
Categorizer class >> defaultList: aSortedCollection [ 
	"Answer an instance of me with initial elements from the argument, 
	aSortedCollection."

	^self new setDefaultList: aSortedCollection
]

{ #category : #documentation }
Categorizer class >> documentation [
	"Instances consist of an Array of category names (categoryArray), each of 
	which refers to an Array of elements (elementArray). This association is 
	made through an Array of stop indices (categoryStops), each of which is 
	the index in elementArray of the last element (if any) of the 
	corresponding category. For example: categories := Array with: 'firstCat' 
	with: 'secondCat' with: 'thirdCat'. stops := Array with: 1 with: 4 with: 4. 
	elements := Array with: #a with: #b with: #c with: #d. This means that 
	category firstCat has only #a, secondCat has #b, #c, and #d, and 
	thirdCat has no elements. This means that stops at: stops size must be the 
	same as elements size." 
]

{ #category : #'class initialization' }
Categorizer class >> initialize [
	"	self  initialize	"
	
	Default := 'as yet unclassified' asSymbol.
	NullCategory := 'no messages' asSymbol.
]

{ #category : #'class initialization' }
Categorizer class >> nullCategory [
	^ NullCategory
]

{ #category : #housekeeping }
Categorizer class >> sortAllCategories [

	self allSubInstances
		do: [:x | x sortCategories]
]

{ #category : #operations }
Categorizer >> addCategory: newCategory [
	^ self addCategory: newCategory before: nil 
]

{ #category : #operations }
Categorizer >> addCategory: catString before: nextCategory [
	"Add a new category named heading.
	If default category exists and is empty, remove it.
	If nextCategory is nil, then add the new one at the end,
	otherwise, insert it before nextCategory."
	| index newCategory |
	newCategory := catString asSymbol.
	(categoryArray indexOf: newCategory) > 0
		ifTrue: [^self].	"heading already exists, so done"
	index := categoryArray indexOf: nextCategory
		ifAbsent: [categoryArray size + 1].
	categoryArray := categoryArray
		copyReplaceFrom: index
		to: index-1
		with: (Array with: newCategory).
	categoryStops := categoryStops
		copyReplaceFrom: index
		to: index-1
		with: (Array with: (index = 1
				ifTrue: [0]
				ifFalse: [categoryStops at: index-1])).
	"remove empty default category"
	(newCategory ~= Default
			and: [(self listAtCategoryNamed: Default) isEmpty])
		ifTrue: [self removeCategory: Default]
]

{ #category : #accessing }
Categorizer >> allMethodSelectors [
	"give a list of all method selectors."

	^ elementArray sorted
]

{ #category : #private }
Categorizer >> assertInvariant [
	[elementArray size = categoryStops last] assert
]

{ #category : #private }
Categorizer >> basicRemoveElement: element [ 
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
	self assertInvariant.
]

{ #category : #accessing }
Categorizer >> categories [
	"Answer an Array of categories (names)."
	categoryArray ifNil: [^ nil].
	(categoryArray size = 1 
		and: [categoryArray first = Default & (elementArray isEmpty)])
		ifTrue: [^Array with: NullCategory].
	^categoryArray
]

{ #category : #accessing }
Categorizer >> categories: anArray [
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
Categorizer >> categoryOfElement: element [ 
	"Answer the category associated with the argument, element."

	| index |
	index := self numberOfCategoryOfElement: element.
	^ index = 0
		ifTrue: [ nil]
		ifFalse: [ categoryArray at: index ]
]

{ #category : #operations }
Categorizer >> changeFromCategorySpecs: categorySpecs [
	"Tokens is an array of categorySpecs as scanned from a browser 'reorganize' pane, or built up by some other process, such as a scan of an environment."

	| newCategories newStops temp cc currentStop oldElements newElements |
	oldElements := elementArray asSet.
	newCategories := Array new: categorySpecs size.
	newStops := Array new: categorySpecs size.
	currentStop := 0.
	newElements := (Array new: 16) writeStream.
	1 to: categorySpecs size do: [ :i | 
		| selectors catSpec |
		catSpec := categorySpecs at: i.
		newCategories at: i put: catSpec first asSymbol.
		selectors := catSpec allButFirst
			collect: [ :each | 
				each isSymbol
					ifTrue: [ each ]
					ifFalse: [ each printString asSymbol ] ].
		selectors asSortedCollection
			do: [ :elem | 
				(oldElements remove: elem ifAbsent: [ nil ]) notNil
					ifTrue: [ 
						newElements nextPut: elem.
						currentStop := currentStop + 1 ] ].
		newStops at: i put: currentStop ].	"Ignore extra elements but don't lose any existing elements!"
	oldElements := oldElements collect: [ :elem | Array with: (self categoryOfElement: elem) with: elem ].
	newElements := newElements contents.
	categoryArray := newCategories.
	(cc := categoryArray asSet) size = categoryArray size
		ifFalse: [ 
			"has duplicate element"
			temp := categoryArray asOrderedCollection.
			temp removeAll: categoryArray asSet.
			temp
				do: [ :dup | 
					| tmp ii |
					tmp := dup.
					ii := categoryArray indexOf: tmp.
					[ 
					tmp := (tmp , ' #2') asSymbol.
					cc includes: tmp ] whileTrue.
					cc add: tmp.
					categoryArray at: ii put: tmp ] ].
	categoryStops := newStops.
	elementArray := newElements.
	oldElements do: [ :pair | self classify: pair last under: pair first ]
]

{ #category : #operations }
Categorizer >> classify: element under: heading [ 
	self classify: element under: heading suppressIfDefault: true
]

{ #category : #operations }
Categorizer >> classify: element under: heading suppressIfDefault: aBoolean [
	"Store the argument, element, in the category named heading.   If aBoolean is true, then invoke special logic such that the classification is NOT done if the new heading is the Default and the element already had a non-Default classification -- useful for filein"

	| catName catIndex elemIndex realHeading |
	((heading = NullCategory) or: [heading == nil])
		ifTrue: [realHeading := Default]
		ifFalse: [realHeading := heading asSymbol].
	(catName := self categoryOfElement: element) = realHeading
		ifTrue: [^ self].  "done if already under that category"

	catName ~~ nil ifTrue: 
		[(aBoolean and: [realHeading = Default])
				ifTrue: [^ self].	  "return if non-Default category already assigned in memory"
		self basicRemoveElement: element].	"remove if in another category"

	(categoryArray indexOf: realHeading) = 0 ifTrue: [self addCategory: realHeading].

	catIndex := categoryArray indexOf: realHeading.
	elemIndex := 
		catIndex > 1
			ifTrue: [categoryStops at: catIndex - 1]
			ifFalse: [0].
	[(elemIndex := elemIndex + 1) <= (categoryStops at: catIndex) 
		and: [element >= (elementArray at: elemIndex)]] whileTrue.

	"elemIndex is now the index for inserting the element. Do the insertion before it."
	elementArray := elementArray copyReplaceFrom: elemIndex to: elemIndex-1
						with: (Array with: element).

	"add one to stops for this and later categories"
	catIndex to: categoryArray size do: 
		[:i | categoryStops at: i put: (categoryStops at: i) + 1].

	((categoryArray includes: Default)
		and: [(self listAtCategoryNamed: Default) size = 0]) ifTrue: [self removeCategory: Default].
		
	self assertInvariant.
]

{ #category : #operations }
Categorizer >> classifyAll: aCollection under: heading [

	aCollection do:
		[:element | self classify: element under: heading]
]

{ #category : #private }
Categorizer >> elementCategoryDict [
	| dict firstIndex lastIndex |
	elementArray ifNil: [^ nil].
	dict := Dictionary new: elementArray size.
	1to: categoryStops size do: [:cat |
		firstIndex := self firstIndexOfCategoryNumber: cat.
		lastIndex := self lastIndexOfCategoryNumber: cat.
		firstIndex to: lastIndex do: [:el |
			dict at: (elementArray at: el) put: (categoryArray at: cat)].
	].
	^ dict.
]

{ #category : #private }
Categorizer >> firstIndexOfCategoryNumber: anInteger [
	anInteger < 1 ifTrue: [^ nil].
	^ (anInteger > 1
			ifTrue: [(categoryStops at: anInteger - 1) + 1]
			ifFalse: [1]).
]

{ #category : #testing }
Categorizer >> isEmptyCategoryNamed: categoryName [
	| i |
	i := categoryArray indexOf: categoryName ifAbsent: [^false].
	^self isEmptyCategoryNumber: i
]

{ #category : #testing }
Categorizer >> isEmptyCategoryNumber: anInteger [

	| firstIndex lastIndex |
	(anInteger < 1 or: [anInteger > categoryStops size])
		ifTrue: [^ true].
	firstIndex := self firstIndexOfCategoryNumber: anInteger.
	lastIndex :=  self lastIndexOfCategoryNumber: anInteger.
	^ firstIndex > lastIndex
]

{ #category : #private }
Categorizer >> lastIndexOfCategoryNumber: anInteger [
	anInteger > categoryStops size ifTrue: [^ nil].
	^ categoryStops at: anInteger
]

{ #category : #queries }
Categorizer >> listAtCategoryNamed: categoryName [
	"Answer the array of elements associated with the name, categoryName."

	| i |
	i := categoryArray indexOf: categoryName ifAbsent: [^Array new].
	^self listAtCategoryNumber: i
]

{ #category : #private }
Categorizer >> listAtCategoryNumber: anInteger [ 
	"Answer the array of elements stored at the position indexed by anInteger.  Answer nil if anInteger is larger than the number of categories."

	| firstIndex lastIndex |
	(anInteger < 1 or: [anInteger > categoryStops size])
		ifTrue: [^ nil].
	firstIndex := self firstIndexOfCategoryNumber: anInteger.
	lastIndex :=  self lastIndexOfCategoryNumber: anInteger.
	^elementArray copyFrom: firstIndex to: lastIndex
]

{ #category : #'queries - protocol' }
Categorizer >> listAtProtocolNamed: categoryName [
	"when the receiver is used to sort methods, it is nicer to use the right vocabulary."
	^ self listAtCategoryNamed: categoryName
]

{ #category : #private }
Categorizer >> numberOfCategoryOfElement: element [
	"Answer the index of the category with which the argument, element, is 
	associated."

	| indexOfElementInElementArray |
	indexOfElementInElementArray := elementArray
		                                identityIndexOf: element asSymbol
		                                ifAbsent: [ ^ 0 ].
	^ categoryStops findFirst: [ :stopIndex | 
		  stopIndex >= indexOfElementInElementArray ]
]

{ #category : #'queries - protocol' }
Categorizer >> protocolOfElement: element [
	"when the receiver is used to sort methods, it is nicer to use the right vocabulary."
	
	^ self categoryOfElement: element 
]

{ #category : #operations }
Categorizer >> removeCategory: cat [ 
	"Remove the category named, cat. Create an error notificiation if the 
	category has any elements in it."

	| index lastStop |
	index := categoryArray indexOf: cat ifAbsent: [^self].
	lastStop := 
		index = 1
			ifTrue: [0]
			ifFalse: [categoryStops at: index - 1].
	(categoryStops at: index) - lastStop > 0 
		ifTrue: [^self error: 'cannot remove non-empty category ', cat].
	categoryArray := categoryArray copyReplaceFrom: index to: index with: Array new.
	categoryStops := categoryStops copyReplaceFrom: index to: index with: Array new.
	categoryArray isEmpty
		ifTrue:
			[categoryArray := Array with: Default.
			categoryStops := Array with: 0]

]

{ #category : #operations }
Categorizer >> removeElement: element [ 
	^ self basicRemoveElement: element
]

{ #category : #operations }
Categorizer >> removeEmptyCategories [
	"Remove empty categories."

	| categoryIndex currentStop keptCategories keptStops |
	keptCategories := (Array new: 16) writeStream.
	keptStops := (Array new: 16) writeStream.
	currentStop := categoryIndex := 0.
	[(categoryIndex := categoryIndex + 1) <= categoryArray size]
		whileTrue: 
			[(categoryStops at: categoryIndex) > currentStop
				ifTrue: 
					[keptCategories nextPut: (categoryArray at: categoryIndex).
					keptStops nextPut: (currentStop := categoryStops at: categoryIndex)]].
	categoryArray := keptCategories contents.
	categoryStops := keptStops contents.
	categoryArray isEmpty
		ifTrue:
			[categoryArray := Array with: Default.
			categoryStops := Array with: 0]

	"ClassOrganizer allInstancesDo: [:co | co removeEmptyCategories]."
]

{ #category : #operations }
Categorizer >> renameCategory: oldCatString toBe: newCatString [
	"Rename a category. No action if new name already exists, or if old name does not exist."
	| index oldCategory newCategory |
	oldCategory := oldCatString asSymbol.
	newCategory := newCatString asSymbol.
	(categoryArray indexOf: newCategory) > 0
		ifTrue: [^ self].	"new name exists, so no action"
	(index := categoryArray indexOf: oldCategory) = 0
		ifTrue: [^ self].	"old name not found, so no action"
	categoryArray := categoryArray copy.  "need to change identity so smart list update will notice the change"
	categoryArray at: index put: newCategory
]

{ #category : #private }
Categorizer >> setDefaultList: aSortedCollection [

	categoryArray := Array with: Default.
	categoryStops := Array with: aSortedCollection size.
	elementArray := aSortedCollection asArray
]

{ #category : #operations }
Categorizer >> sortCategories [
	| privateCategories publicCategories newCategories |
	privateCategories := self categories
		select: [ :one | (one findString: 'private' startingAt: 1 caseSensitive: false) = 1 ].
	publicCategories := self categories copyWithoutAll: privateCategories.
	newCategories := publicCategories asSortedCollection asOrderedCollection
		addAll: privateCategories;
		asArray.
	self categories: newCategories
]

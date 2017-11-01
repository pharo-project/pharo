"
I am a history entry which hold a single step of the Nautilus browsing history
"
Class {
	#name : #NautilusHistoryEntry,
	#superclass : #Object,
	#instVars : [
		'selectedPackage',
		'selectedCategory',
		'selectedSelector',
		'selectedClass',
		'selectedGroup',
		'showHierarchy',
		'sortHierarchically',
		'showComment',
		'showInstance',
		'showCategories'
	],
	#category : 'NautilusCommon'
}

{ #category : #'instance creation' }
NautilusHistoryEntry class >> with: aNautilus [

	^ self new 
		with: aNautilus;
		yourself
]

{ #category : #comparing }
NautilusHistoryEntry >> = anotherWrapper [
	self species = anotherWrapper species
		ifFalse: [ ^ false ].
	^ self selectedPackage = anotherWrapper selectedPackage
		and:
			[ 
			self selectedGroup = anotherWrapper selectedGroup
				and:
					[ 
					self selectedClass = anotherWrapper selectedClass
						and:
							[ 
							self selectedCategory = anotherWrapper selectedCategory
								and:
									[ 
									self selectedSelector = anotherWrapper selectedSelector
										and:
											[ 
											self showHierarchy = anotherWrapper showHierarchy
												and:
													[ 
													self showInstance = anotherWrapper showInstance
														and:
															[ 
															self sortHierarchically = anotherWrapper sortHierarchically
																and:
																	[  
																	self showComment = anotherWrapper showComment
																		and: [ self showCategories = anotherWrapper showCategories ] ] ] ] ] ] ] ] ]
]

{ #category : #protocol }
NautilusHistoryEntry >> applyTo: aNautilus [
	aNautilus ui ifNotNil: [ :interface | interface resetAllListSelections ].
	aNautilus
		selectedPackage: self selectedPackage;
		selectedClass: selectedClass;
		selectedCategory: self selectedCategory.
	aNautilus selectedMethod: self selectedMethod.
	aNautilus browsingHistory
		pauseDuring: [ 
			aNautilus
				selectedGroup: self selectedGroup;
				showHierarchy: self showHierarchy;
				sortHierarchically: self sortHierarchically;
				showComment: self showComment;
				showInstance: self showInstance;
				showCategories: self showCategories ].
	aNautilus ui
		ifNotNil: [ :interface | 
			interface showHierarchy: aNautilus showHierarchy.
			interface showInstance: (selectedClass ifNil: [ false ] ifNotNil: [ selectedClass isMeta not ]).
			interface
				selectedPackageWithoutChangingSelectionInternally: self selectedPackage;
				selectedClassWithoutChangingSelectionInternally: selectedClass;
				selectedCategoryInternally: self selectedCategory.
			interface updatePackageGroupAndClassList.
			interface selectedMethod: self selectedMethod.
			interface updateCategoryAndMethodList ]
]

{ #category : #protocol }
NautilusHistoryEntry >> asHistoryString [
	| separator |
	separator := ' >>#'.
	^ String
		streamContents:
			[ :s | 
			self selectedPackage
				ifNotNil:
					[ :p | 
					self selectedClass
						ifNotNil:
							[ :c | 
							s << c name.
							self selectedSelector ifNotNil: [ :m | s << separator << m asString ] ] ] ]
]

{ #category : #comparing }
NautilusHistoryEntry >> hash [
	| hash |
	hash := selectedPackage hash.
	{ selectedCategory . selectedSelector . selectedClass . selectedGroup } do: [ :iv |
		hash := hash bitXor: iv hash ].
	^ hash
]

{ #category : #accessing }
NautilusHistoryEntry >> selectedCategory [

		^ selectedCategory
]

{ #category : #accessing }
NautilusHistoryEntry >> selectedCategory: anObject [
	
		selectedCategory := anObject
]

{ #category : #accessing }
NautilusHistoryEntry >> selectedClass [

		^ selectedClass
]

{ #category : #accessing }
NautilusHistoryEntry >> selectedClass: anObject [
	
		selectedClass := anObject
]

{ #category : #accessing }
NautilusHistoryEntry >> selectedGroup [

		^ selectedGroup
]

{ #category : #accessing }
NautilusHistoryEntry >> selectedGroup: anObject [
	
		selectedGroup := anObject
]

{ #category : #accessing }
NautilusHistoryEntry >> selectedMethod [
	selectedSelector ifNil: [ ^ nil ].
	^ selectedClass ifNil: [ nil ] ifNotNil: [ selectedClass compiledMethodAt: selectedSelector ifAbsent: [ nil ] ]
]

{ #category : #accessing }
NautilusHistoryEntry >> selectedMethod: anObject [
	
		selectedSelector := anObject
]

{ #category : #accessing }
NautilusHistoryEntry >> selectedPackage [

		^ selectedPackage
]

{ #category : #accessing }
NautilusHistoryEntry >> selectedPackage: anObject [
	
		selectedPackage := anObject
]

{ #category : #accessing }
NautilusHistoryEntry >> selectedSelector [

	^ selectedSelector
]

{ #category : #accessing }
NautilusHistoryEntry >> selectedSelector: anObject [
	
		selectedSelector := anObject
]

{ #category : #accessing }
NautilusHistoryEntry >> showCategories [

		^ showCategories
]

{ #category : #accessing }
NautilusHistoryEntry >> showCategories: anObject [
	
		showCategories := anObject
]

{ #category : #accessing }
NautilusHistoryEntry >> showComment [

		^ showComment
]

{ #category : #accessing }
NautilusHistoryEntry >> showComment: anObject [
	
		showComment := anObject
]

{ #category : #accessing }
NautilusHistoryEntry >> showHierarchy [

		^ showHierarchy
]

{ #category : #accessing }
NautilusHistoryEntry >> showHierarchy: anObject [
	
		showHierarchy:= anObject
]

{ #category : #accessing }
NautilusHistoryEntry >> showInstance [

		^ showInstance
]

{ #category : #accessing }
NautilusHistoryEntry >> showInstance: anObject [
	
		showInstance := anObject
]

{ #category : #accessing }
NautilusHistoryEntry >> sortHierarchically [

		^ sortHierarchically
]

{ #category : #accessing }
NautilusHistoryEntry >> sortHierarchically: anObject [
	
		sortHierarchically := anObject
]

{ #category : #private }
NautilusHistoryEntry >> with: aNautilus [
	
	self 
		selectedPackage: aNautilus selectedPackage;
		selectedCategory: aNautilus selectedCategory.
		aNautilus selectedMethod
			ifNil: [ self selectedSelector: nil ]
			ifNotNil: [:met | self selectedSelector: met selector].
	self
		selectedClass: aNautilus selectedClass;
		selectedGroup: aNautilus selectedGroup;
		showHierarchy: aNautilus showHierarchy;
		sortHierarchically: aNautilus sortHierarchically;
		showComment: aNautilus showComment;
		showInstance: aNautilus showInstance;
		showCategories: aNautilus showCategories.
]

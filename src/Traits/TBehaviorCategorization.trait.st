"
Kernel trait for Behavior categorization
"
Trait {
	#name : #TBehaviorCategorization,
	#category : 'Traits-Kernel-Traits'
}

{ #category : #organization }
TBehaviorCategorization >> category [
	"Answer the system organization category for the receiver. First check whether the
	category name stored in the ivar is still correct and only if this fails look it up
	(latter is much more expensive)"

	| result |
	self basicCategory ifNotNil: [ :symbol |
		((self environment organization listAtCategoryNamed: symbol) includes: self name)
			ifTrue: [ ^symbol ] ].
	result := (self environment organization categoryOfElement: self name)
		ifNil: [ #Unclassified ]
		ifNotNil: [:aString | aString ].
	self basicCategory: result.
	
	^ result
]

{ #category : #organization }
TBehaviorCategorization >> category: aString [ 
	"Categorize the receiver under the system category, aString, removing it from 
	any previous categorization."

	| oldCategory |
	oldCategory := self basicCategory.
	aString isString
		ifTrue: [
			self basicCategory: aString asSymbol.
			self environment organization classify: self name under: self basicCategory ]
		ifFalse: [self errorCategoryName].
	SystemAnnouncer uniqueInstance
		class: self recategorizedFrom: oldCategory to: self basicCategory
]

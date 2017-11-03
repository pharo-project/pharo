Class {
	#name : #EpRevertTest,
	#superclass : #EpLogBrowserOperationFactoryTest,
	#category : #EpiceaBrowsersTests-Integration
}

{ #category : #private }
EpRevertTest >> revertInputEntry [

	EpLogBrowserOperationFactory new
		logBrowserModel: (EpLogBrowserModel newWithLog: monitor log);
		entries: { inputEntry };
		errorHandlerBlock: [:error | error signal ];
		revertCodeChanges
]

{ #category : #tests }
EpRevertTest >> testBehaviorCategoryChange [

	| aClass aCategory anotherCategory |
	aClass := classFactory newClass.
	aCategory := aClass category.
	anotherCategory := aCategory, '2'.
	aClass category: anotherCategory.
	self setHeadAsInputEntry.
	
	self assert: (inputEntry content isKindOf: EpBehaviorCategoryChange).
	self assert: aClass category equals: anotherCategory.
	self revertInputEntry.
	self assert: aClass category equals: aCategory.

]

{ #category : #tests }
EpRevertTest >> testBehaviorCommentChange [

	| aClass |
	aClass := classFactory newClass.
	aClass classComment: 'before'.
	aClass classComment: 'after'.
	self setHeadAsInputEntry.

	self revertInputEntry.
	
	self assert: aClass organization classComment equals: 'before'.

]

{ #category : #tests }
EpRevertTest >> testBehaviorNameChange [

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	aClass rename: className, '2'.
	self setHeadAsInputEntry.

	self revertInputEntry.
	
	self assert: aClass name equals: className.

]

{ #category : #tests }
EpRevertTest >> testCategoryAdditionWithCategoryRemoved [

	| organization aCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	organization addCategory: aCategory.
	self setHeadAsInputEntry.

	self assert: (organization includesCategory: aCategory).
	self revertInputEntry.
	self deny: (organization includesCategory: aCategory).

]

{ #category : #tests }
EpRevertTest >> testCategoryRemovalWithCategoryAdded [

	| organization aCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	organization addCategory: aCategory.
	organization removeCategory: aCategory.
	self setHeadAsInputEntry.

	self deny: (organization includesCategory: aCategory).
	self revertInputEntry.
	self assert: (organization includesCategory: aCategory).

]

{ #category : #tests }
EpRevertTest >> testCategoryRename [

	| organization aCategory anotherCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	anotherCategory := aCategory, '2'.
	organization addCategory: aCategory.
	organization renameCategory: aCategory toBe: anotherCategory.
	self setHeadAsInputEntry.

	self assert: inputEntry content class equals: EpCategoryRename.
	self deny: (organization includesCategory: aCategory).
	self assert: (organization includesCategory: anotherCategory).
	self revertInputEntry.
	self assert: (organization includesCategory: aCategory).
	self deny: (organization includesCategory: anotherCategory).

]

{ #category : #tests }
EpRevertTest >> testClassAddition [

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	self setHeadAsInputEntry.
	
	self assert: (self class environment hasClassNamed: className).
	self revertInputEntry.
	self deny: (self class environment hasClassNamed: className).

]

{ #category : #tests }
EpRevertTest >> testClassAdditionWithClassAlreadyRemoved [

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	self setHeadAsInputEntry.
	
	aClass removeFromSystem.
	
	self deny: (self class environment hasClassNamed: className).
	self revertInputEntry.
	self deny: (self class environment hasClassNamed: className).

]

{ #category : #tests }
EpRevertTest >> testClassModificationWithClassRemoved [

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	aClass addInstVarNamed: #x.
	self setHeadAsInputEntry.
	
	aClass removeFromSystem.
	
	self assert: (inputEntry content isKindOf: EpClassModification).
	self deny: (self class environment hasClassNamed: className).
	self revertInputEntry.
	self 
		assert: (self class environment hasClassNamed: className)
		description: 'Revert of a modification of a class that was removed will add it back with the original look'.
	self
		assert: (self class environment classNamed: className) instVarNames isEmpty
		description: 'The old class of the modification had no instance variables'.

]

{ #category : #tests }
EpRevertTest >> testClassRemoval [

	| aClass className category |
	aClass := classFactory newClass.
	aClass theMetaClass instanceVariableNames: 'hey'.
	className := aClass name.
	category := aClass category.
	aClass removeFromSystem.
	self setHeadAsInputEntry.

	self assert: (inputEntry content isKindOf: EpClassRemoval).
	self deny: (self class environment hasClassNamed: className).
	self revertInputEntry.
	self assert: (self class environment hasClassNamed: className).
	self assert: (self class environment classNamed: className) category equals: category.
	self assert: (self class environment classNamed: className) theMetaClass instVarNames equals: #('hey').
]

{ #category : #tests }
EpRevertTest >> testClassRemovalWithClassAlreadyAdded [

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	aClass removeFromSystem.
	self setHeadAsInputEntry.

	aClass := classFactory newClass.
	aClass rename: className.

	self assert: (inputEntry content isKindOf: EpClassRemoval).
	self assert: (self class environment hasClassNamed: className).
	self revertInputEntry.
	self assert: (self class environment hasClassNamed: className).

]

{ #category : #tests }
EpRevertTest >> testMethodAddition [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	self setHeadAsInputEntry.

	self assert: (inputEntry content isKindOf: EpMethodAddition).
	self assert: (aClass includesSelector: #fortyTwo).
	self revertInputEntry.
	self deny: (aClass includesSelector: #fortyTwo).
]

{ #category : #tests }
EpRevertTest >> testMethodAdditionWithMethodAlreadyRemoved [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	self setHeadAsInputEntry.

	aClass removeSelector: #fortyTwo.
	
	self assert: (inputEntry content isKindOf: EpMethodAddition).
	self deny: (aClass includesSelector: #fortyTwo).
	self revertInputEntry.
	self deny: (aClass includesSelector: #fortyTwo).
]

{ #category : #tests }
EpRevertTest >> testMethodModification [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	aClass compile: 'fortyTwo ^nil'.
	self setHeadAsInputEntry.
	
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^nil'.
	self revertInputEntry.
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^42'.

]

{ #category : #tests }
EpRevertTest >> testMethodModificationWithProtocolChanged [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'one'.
	(aClass>>#fortyTwo) protocol: 'two'.
	self setHeadAsInputEntry.

	(aClass>>#fortyTwo) protocol: 'three'.
	
	self assert: (aClass>>#fortyTwo) protocol equals: 'three'.
	self revertInputEntry.
	self assert: (aClass>>#fortyTwo) protocol equals: 'one'.

]

{ #category : #tests }
EpRevertTest >> testMethodRemoval [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass removeSelector: #fortyTwo.
	self setHeadAsInputEntry.

	self assert: (inputEntry content isKindOf: EpMethodRemoval).
	self deny: (aClass includesSelector: #fortyTwo).
	self revertInputEntry.
	self assert: (aClass includesSelector: #fortyTwo).
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^42'.
	self assert: (aClass>>#fortyTwo) protocol equals: 'number'.

]

{ #category : #tests }
EpRevertTest >> testMethodRemovalWithMethodAlreadyAdded [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass removeSelector: #fortyTwo.
	self setHeadAsInputEntry.

	aClass compile: 'fortyTwo ^nil' classified: 'none'.

	self assert: (inputEntry content isKindOf: EpMethodRemoval).
	self assert: (aClass includesSelector: #fortyTwo).
	self revertInputEntry.
	self assert: (aClass includesSelector: #fortyTwo).
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^42'.
	self assert: (aClass>>#fortyTwo) protocol equals: 'number'.

]

{ #category : #tests }
EpRevertTest >> testProtocolAddition [

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	self setHeadAsInputEntry.

	self assert: inputEntry content class equals: EpProtocolAddition.
	self assert: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').
	self revertInputEntry.
	self deny: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').

]

{ #category : #tests }
EpRevertTest >> testProtocolRemoval [

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	aClass removeProtocol: 'protocol'.
	self setHeadAsInputEntry.

	self assert: inputEntry content class equals: EpProtocolRemoval.
	self deny: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').
	self revertInputEntry.
	self assert: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').

]

{ #category : #tests }
EpRevertTest >> testTraitAddition [

	| aTrait traitName |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	self setHeadAsInputEntry.

	self assert: (self class environment includesKey: traitName).
	self revertInputEntry.
	self deny: (self class environment includesKey: traitName).

]

{ #category : #tests }
EpRevertTest >> testTraitAdditionWithTraitAlreadyRemoved [

	| aTrait traitName |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	self setHeadAsInputEntry.
	
	aTrait removeFromSystem.

	self deny: (self class environment includesKey: traitName).
	self revertInputEntry.
	self deny: (self class environment includesKey: traitName).

]

{ #category : #tests }
EpRevertTest >> testTraitRemoval [

	| aTrait traitName |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	aTrait removeFromSystem.
	self setHeadAsInputEntry.

	self deny: (self class environment includesKey: traitName).
	self revertInputEntry.
	self assert: (self class environment includesKey: traitName).
	"---> Fails to create trait with Unclassified category"
]

{ #category : #tests }
EpRevertTest >> testTraitRemovalWithTraitAlreadyAdded [

	| aTrait traitName |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	aTrait removeFromSystem.
	self setHeadAsInputEntry.

	"Restore trait"
	aTrait := classFactory newTrait.
	aTrait rename: traitName.

	self assert: (self class environment includesKey: traitName).
	self revertInputEntry.
	self assert: (self class environment includesKey: traitName).

]

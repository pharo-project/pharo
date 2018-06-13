Class {
	#name : #EpApplyTest,
	#superclass : #EpLogBrowserOperationFactoryTest,
	#category : #'EpiceaBrowsers-Tests-Integration'
}

{ #category : #tests }
EpApplyTest >> applyInputEntry [

	EpLogBrowserOperationFactory new
		logBrowserModel: (EpLogBrowserModel newWithLog: monitor log);
		entries: { inputEntry };
		errorHandlerBlock: [:error | error signal ];
		applyCodeChanges

]

{ #category : #tests }
EpApplyTest >> testBehaviorNameChange [

	| aClass oldName newName |
	aClass := classFactory newClass.
	oldName := aClass name.
	newName := oldName, '2'.
	aClass rename: newName.
	self setHeadAsInputEntry.

	aClass rename: oldName. "Rollback"

	self assert: aClass name equals: oldName.
	self applyInputEntry.
	self assert: aClass name equals: newName.
]

{ #category : #tests }
EpApplyTest >> testCategoryAdditionWithCategoryRemoved [

	| organization aCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	organization addCategory: aCategory.
	self setHeadAsInputEntry.
	organization removeCategory: aCategory.

	self assert: inputEntry content class equals: EpCategoryAddition.
	self deny: (organization includesCategory: aCategory).
	self applyInputEntry.
	self assert: (organization includesCategory: aCategory).

]

{ #category : #tests }
EpApplyTest >> testCategoryRemovalWithCategoryAdded [

	| organization aCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	organization addCategory: aCategory.
	organization removeCategory: aCategory.
	self setHeadAsInputEntry.
	organization addCategory: aCategory.

	self assert: inputEntry content class equals: EpCategoryRemoval.
	self assert: (organization includesCategory: aCategory).
	self applyInputEntry.
	self deny: (organization includesCategory: aCategory).

]

{ #category : #tests }
EpApplyTest >> testCategoryRename [

	| organization aCategory anotherCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	anotherCategory := aCategory, '2'.
	organization addCategory: aCategory.
	organization renameCategory: aCategory toBe: anotherCategory.
	self setHeadAsInputEntry.
	organization renameCategory: anotherCategory toBe: aCategory. "Rollback"

	self assert: inputEntry content class equals: EpCategoryRename.
	self assert: (organization includesCategory: aCategory).
	self deny: (organization includesCategory: anotherCategory).
	self applyInputEntry.
	self deny: (organization includesCategory: aCategory).
	self assert: (organization includesCategory: anotherCategory).

]

{ #category : #tests }
EpApplyTest >> testClassAdditionWithCategoryChanged [

	| aClass aCategory anotherCategory |
	aClass := classFactory newClass.
	self setHeadAsInputEntry.

	aCategory := aClass category.
	anotherCategory := aCategory, '2'.
	aClass category: anotherCategory.
	
	self assert: aClass category equals: anotherCategory.
	self applyInputEntry.
	self assert: aClass category equals: aCategory
]

{ #category : #tests }
EpApplyTest >> testClassAdditionWithClassRemoved [

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	self setHeadAsInputEntry.
	
	aClass removeFromSystem.
	
	self deny: (self class environment hasClassNamed: className).
	self applyInputEntry.
	self assert: (self class environment hasClassNamed: className).

]

{ #category : #tests }
EpApplyTest >> testClassAdditionWithInstanceVariablesChanged [

	| aClass |
	aClass := classFactory newClass.
	self setHeadAsInputEntry.
	
	aClass addInstVarNamed: #x.
	
	self assert: aClass instVarNames equals: #(x).
	self applyInputEntry.
	self assert: aClass instVarNames equals: #()
]

{ #category : #tests }
EpApplyTest >> testClassAdditionWithMetaclassInstanceVariablesChanged [

	| aClass |
	aClass := classFactory newClass.
	self setHeadAsInputEntry.
	
	aClass classSide instanceVariableNames: 'x'.
	
	self assert: aClass classSide instVarNames equals: #(x).
	self applyInputEntry.
	self assert: aClass classSide instVarNames equals: #()
]

{ #category : #tests }
EpApplyTest >> testClassAdditionWithSuperclassChanged [

	| aClass |
	aClass := classFactory newClass.
	self setHeadAsInputEntry.
	
	aClass superclass: Array.
	
	self assert: aClass superclass equals: Array.
	self applyInputEntry.
	self assert: aClass superclass equals: Object
]

{ #category : #tests }
EpApplyTest >> testClassModificationWithClassRemoved [

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	aClass addInstVarNamed: #x.
	self setHeadAsInputEntry.
	
	aClass removeFromSystem.
	
	self assert: (inputEntry content isKindOf: EpClassModification).
	self deny: (self class environment hasClassNamed: className).
	self applyInputEntry.
	self assert: (self class environment hasClassNamed: className).
	self assert: aClass instVarNames equals: #(x).

]

{ #category : #tests }
EpApplyTest >> testClassRemovalWithClassAdded [

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	aClass removeFromSystem.
	self setHeadAsInputEntry.

	aClass := classFactory newClass.
	aClass rename: className.

	self assert: (inputEntry content isKindOf: EpClassRemoval).
	self assert: (self class environment hasClassNamed: className).
	self applyInputEntry.
	self deny: (self class environment hasClassNamed: className).

]

{ #category : #tests }
EpApplyTest >> testMethodAdditionWithMethodRemoved [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.

	aClass removeSelector: #fortyTwo.
	
	self assert: (inputEntry content isKindOf: EpMethodAddition).
	self deny: (aClass includesSelector: #fortyTwo).
	self applyInputEntry.
	self assert: (aClass includesSelector: #fortyTwo).
	self assert: (aClass>>#fortyTwo) protocol equals: 'number'.

]

{ #category : #tests }
EpApplyTest >> testMethodAdditionWithProtocolChanged [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.

	(aClass>>#fortyTwo) protocol: 'none'.
	
	self assert: (aClass>>#fortyTwo) protocol equals: 'none'.
	self applyInputEntry.
	self assert: (aClass>>#fortyTwo) protocol equals: 'number'.

]

{ #category : #tests }
EpApplyTest >> testMethodAdditionWithSourceCodeChanged [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	self setHeadAsInputEntry.

	aClass compile: 'fortyTwo ^nil'.
	
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^nil'.
	self applyInputEntry.
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^42'.

]

{ #category : #tests }
EpApplyTest >> testMethodModificationWithMethodRemoved [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass compile: 'fortyTwo ^nil' classified: 'number'.
	self setHeadAsInputEntry.

	aClass removeSelector: #fortyTwo.

	self assert: (inputEntry content isKindOf: EpMethodModification).
	self deny: (aClass includesSelector: #fortyTwo).
	self applyInputEntry.
	self assert: (aClass includesSelector: #fortyTwo).
	self assert: (aClass>>#fortyTwo) protocol equals: 'number'.

]

{ #category : #tests }
EpApplyTest >> testMethodRemovalWithMethodAdded [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass removeSelector: #fortyTwo.
	self setHeadAsInputEntry.

	aClass compile: 'fortyTwo ^42' classified: 'number'.

	self assert: (inputEntry content isKindOf: EpMethodRemoval).
	self assert: (aClass includesSelector: #fortyTwo).
	self applyInputEntry.
	self deny: (aClass includesSelector: #fortyTwo).

]

{ #category : #tests }
EpApplyTest >> testProtocolAddition [

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	self setHeadAsInputEntry.
	aClass removeProtocol: 'protocol'.

	self assert: inputEntry content class equals: EpProtocolAddition.
	self deny: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').
	self applyInputEntry.
	self assert: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').

]

{ #category : #tests }
EpApplyTest >> testProtocolRemoval [

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	aClass removeProtocol: 'protocol'.
	self setHeadAsInputEntry.
	aClass organization addCategory: 'protocol'.

	self assert: inputEntry content class equals: EpProtocolRemoval.
	self assert: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').
	self applyInputEntry.
	self deny: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').

]

{ #category : #tests }
EpApplyTest >> testTraitAdditionWithTraitRemoved [

	| aTrait traitName |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	self setHeadAsInputEntry.
	
	aTrait removeFromSystem.

	self deny: (self class environment includesKey: traitName).
	self applyInputEntry.
	self assert: (self class environment includesKey: traitName).

]

{ #category : #tests }
EpApplyTest >> testTraitModification [
	| aTrait traitName anotherTrait |
	aTrait := classFactory newTrait.
	anotherTrait := classFactory newTrait.
	traitName := aTrait name.
	aTrait setTraitComposition: anotherTrait asTraitComposition.

	"That should trigger a EpTraitModification and do not trigger many method events."


	self setHeadAsInputEntry.

	self assert: inputEntry content class equals: EpTraitModification.
	self applyInputEntry.
	self assert: (self class environment includesKey: traitName).

]

{ #category : #tests }
EpApplyTest >> testTraitRemovalWithTraitAdded [

	| aTrait traitName |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	aTrait removeFromSystem.
	self setHeadAsInputEntry.

	aTrait := classFactory newTrait.
	aTrait rename: traitName.

	self assert: (inputEntry content isKindOf: EpTraitRemoval).
	self assert: (self class environment includesKey: traitName).
	self applyInputEntry.
	self deny: (self class environment includesKey: traitName).

]

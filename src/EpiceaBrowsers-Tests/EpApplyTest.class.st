Class {
	#name : #EpApplyTest,
	#superclass : #EpLogBrowserOperationFactoryTest,
	#category : #'EpiceaBrowsers-Tests-Integration'
}

{ #category : #helpers }
EpApplyTest >> applyInputEntry [

	EpLogBrowserOperationFactory new
		logBrowserModel: (EpLogBrowserPresenter newWithLog: monitor log);
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
	self assert: aClass name equals: newName
]

{ #category : #tests }
EpApplyTest >> testCategoryRemovalWithPackageTagAddition [

	| aCategory |
	aCategory := classFactory defaultCategory.
	
	self packageOrganizer ensureTagNamed: classFactory defaultTagPostfix inPackageNamed: classFactory packageName.
	self packageOrganizer removeCategory: aCategory.
	self setHeadAsInputEntry.
	self packageOrganizer ensureTagNamed: classFactory defaultTagPostfix inPackageNamed: classFactory packageName.

	self assert: inputEntry content class equals: EpPackageTagRemoval.
	self assert: (self packageOrganizer includesCategory: aCategory).
	self applyInputEntry.
	self deny: (self packageOrganizer includesCategory: aCategory)
]

{ #category : #tests }
EpApplyTest >> testCategoryRename [

	| aCategory anotherCategory |
	aCategory := classFactory defaultCategory.
	anotherCategory := aCategory , '2'.

	self packageOrganizer ensureTagNamed: classFactory defaultTagPostfix inPackageNamed: classFactory packageName.
	self packageOrganizer renameCategory: aCategory toBe: anotherCategory.
	self setHeadAsInputEntry.
	self packageOrganizer renameCategory: anotherCategory toBe: aCategory. "Rollback"

	self assert: inputEntry content class equals: EpCategoryRename.
	self assert: (self packageOrganizer includesCategory: aCategory).
	self deny: (self packageOrganizer includesCategory: anotherCategory).
	self applyInputEntry.
	self deny: (self packageOrganizer includesCategory: aCategory).
	self assert: (self packageOrganizer includesCategory: anotherCategory)
]

{ #category : #tests }
EpApplyTest >> testCategoryRenameWithClass [
	"Let's rename a package with a class and see if the class has the right package after."

	| organization aCategory anotherCategory aClass |
	organization := self packageOrganizer.
	aCategory := classFactory defaultCategory.
	anotherCategory := aCategory , '2'.
	
	self packageOrganizer ensureTagNamed: classFactory defaultTagPostfix inPackageNamed: classFactory packageName.

	aClass := classFactory newClass.
	aClass category: aCategory.

	organization renameCategory: aCategory toBe: anotherCategory.
	self setHeadAsInputEntry.

	organization renameCategory: anotherCategory toBe: aCategory. "Rollback"

	self assert: inputEntry content class equals: EpCategoryRename.
	self assert: aClass category equals: aCategory.
	self applyInputEntry.
	self assert: aClass category equals: anotherCategory
]

{ #category : #tests }
EpApplyTest >> testCategoryRenameWithExtension [
	"Let's rename a package with a class and see if the class has the right package after."

	| organization aCategory anotherCategory aClass |
	organization := self packageOrganizer.
	aCategory := classFactory defaultCategory.
	anotherCategory := aCategory , '2'.
	organization addCategory: aCategory.

	aClass := classFactory newClass.
	aClass category: aCategory , '3'.
	aClass compile: 'fortyTwo ^42' classified: '*' , aCategory.

	organization renameCategory: aCategory toBe: anotherCategory.
	self setHeadAsInputEntry.

	self assert: (aClass >> #fortyTwo) protocolName equals: '*' , anotherCategory.
	organization renameCategory: anotherCategory toBe: aCategory. "Rollback"

	self assert: inputEntry content class equals: EpCategoryRename.
	self assert: (aClass >> #fortyTwo) protocolName equals: '*' , aCategory.
	self applyInputEntry.
	self assert: (aClass >> #fortyTwo) protocolName equals: '*' , anotherCategory
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
	self assert: (self class environment hasClassNamed: className)
]

{ #category : #tests }
EpApplyTest >> testClassAdditionWithCommentChanged [

	| aClass |
	aClass := classFactory newClass.
	aClass comment: 'First comment'.
	self setHeadAsInputEntry.

	aClass comment: 'Second comment'.

	self assert: aClass comment equals: 'Second comment'.
	self applyInputEntry.
	self assert: aClass comment equals: 'First comment'
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
	self assert: aClass instVarNames equals: #(x)
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
	self deny: (self class environment hasClassNamed: className)
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
	self assert: (aClass>>#fortyTwo) protocolName equals: 'number'
]

{ #category : #tests }
EpApplyTest >> testMethodAdditionWithProtocolChanged [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.

	aClass >> #fortyTwo protocol: 'none'.

	self assert: (aClass >> #fortyTwo) protocolName equals: 'none'.
	self applyInputEntry.
	self assert: (aClass >> #fortyTwo) protocolName equals: 'number'
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
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^42'
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
	self assert: (aClass >> #fortyTwo) protocolName equals: 'number'
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
	self deny: (aClass includesSelector: #fortyTwo)
]

{ #category : #tests }
EpApplyTest >> testPackageAddition [

	[
	self packageOrganizer ensurePackage: classFactory packageName.
	self setHeadAsInputEntry.
	self packageOrganizer removePackage: classFactory packageName.

	self assert: inputEntry content class equals: EpPackageAddition.
	self deny: (self packageOrganizer hasPackage: classFactory packageName).
	self applyInputEntry.
	self assert: (self packageOrganizer hasPackage: classFactory packageName) ] ensure: [ self packageOrganizer removePackage: classFactory packageName ]
]

{ #category : #tests }
EpApplyTest >> testPackageRemoval [

	[
	self packageOrganizer ensurePackage: classFactory packageName.
	self packageOrganizer removePackage: classFactory packageName.
	self setHeadAsInputEntry.
	self packageOrganizer ensurePackage: classFactory packageName.

	self assert: inputEntry content class equals: EpPackageRemoval.
	self assert: (self packageOrganizer hasPackage: classFactory packageName).
	self applyInputEntry.
	self deny: (self packageOrganizer hasPackage: classFactory packageName) ] ensure: [ self packageOrganizer removePackage: classFactory packageName ]
]

{ #category : #tests }
EpApplyTest >> testPackageTagAdditionWithCategoryRemoved [

	| aCategory |
	aCategory := classFactory defaultCategory.
	
	self packageOrganizer ensureTagNamed: classFactory defaultTagPostfix inPackageNamed: classFactory packageName.
	self setHeadAsInputEntry.
	self packageOrganizer removeCategory: aCategory.

	self assert: inputEntry content class equals: EpPackageTagAddition.
	self deny: (self packageOrganizer includesCategory: aCategory).
	self applyInputEntry.
	self assert: (self packageOrganizer includesCategory: aCategory)
]

{ #category : #tests }
EpApplyTest >> testProtocolAddition [

	| aClass |
	aClass := classFactory newClass.
	aClass addProtocol: 'protocol'.
	self setHeadAsInputEntry.
	aClass removeProtocol: 'protocol'.

	self assert: inputEntry content class equals: EpProtocolAddition.
	self deny: (aClass hasProtocol: 'protocol').
	self applyInputEntry.
	self assert: (aClass hasProtocol: 'protocol')
]

{ #category : #tests }
EpApplyTest >> testProtocolRemoval [

	| aClass |
	aClass := classFactory newClass.
	aClass addProtocol: 'protocol'.
	aClass removeProtocol: 'protocol'.
	self setHeadAsInputEntry.
	aClass addProtocol: 'protocol'.

	self assert: inputEntry content class equals: EpProtocolRemoval.
	self assert: (aClass hasProtocol: 'protocol').
	self applyInputEntry.
	self deny: (aClass hasProtocol: 'protocol')
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
	self assert: (self class environment includesKey: traitName)
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
	self assert: (self class environment includesKey: traitName)
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
	self deny: (self class environment includesKey: traitName)
]

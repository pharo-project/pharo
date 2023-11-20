Class {
	#name : 'EpApplyTest',
	#superclass : 'EpLogBrowserOperationFactoryTest',
	#category : 'EpiceaBrowsers-Tests-Integration',
	#package : 'EpiceaBrowsers-Tests',
	#tag : 'Integration'
}

{ #category : 'helpers' }
EpApplyTest >> applyInputEntry [

	EpLogBrowserOperationFactory new
		logBrowserModel: (EpLogBrowserPresenter newWithLog: monitor log);
		entries: { inputEntry };
		errorHandlerBlock: [:error | error signal ];
		applyCodeChanges
]

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
EpApplyTest >> testClassAdditionWithInstanceVariablesChanged [

	| aClass |
	aClass := classFactory newClass.
	self setHeadAsInputEntry.

	aClass addInstVarNamed: #x.

	self assert: aClass instVarNames equals: #(x).
	self applyInputEntry.
	self assert: aClass instVarNames equals: #()
]

{ #category : 'tests' }
EpApplyTest >> testClassAdditionWithMetaclassInstanceVariablesChanged [

	| aClass |
	aClass := classFactory newClass.
	self setHeadAsInputEntry.

	aClass classSide instanceVariableNames: 'x'.

	self assert: aClass classSide instVarNames equals: #(x).
	self applyInputEntry.
	self assert: aClass classSide instVarNames equals: #()
]

{ #category : 'tests' }
EpApplyTest >> testClassAdditionWithPackageChanged [

	| aClass package tag newPackage newTag |
	aClass := classFactory newClass.
	self setHeadAsInputEntry.

	package := aClass package name.
	tag := aClass packageTag name.
	newPackage := package , '2'.
	newTag := tag , '2'.

	aClass package: newPackage tag: newTag.

	self assert: aClass package name equals: newPackage.
	self assert: aClass packageTag name equals: newTag.
	self applyInputEntry.
	self assert: aClass package name equals: package.
	self assert: aClass packageTag name equals: tag
]

{ #category : 'tests' }
EpApplyTest >> testClassAdditionWithSuperclassChanged [

	| aClass |
	aClass := classFactory newClass.
	self setHeadAsInputEntry.

	aClass superclass: Array.

	self assert: aClass superclass equals: Array.
	self applyInputEntry.
	self assert: aClass superclass equals: Object
]

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
EpApplyTest >> testPackageRename [

	self packageOrganizer ensurePackage: classFactory packageName.
	self packageOrganizer renamePackage: classFactory packageName to: classFactory packageName , '2'.
	self setHeadAsInputEntry.

	self packageOrganizer renamePackage: classFactory packageName , '2' to: classFactory packageName. "Rollback"

	self assert: inputEntry content class equals: EpPackageRename.
	self assert: (self packageOrganizer hasPackage: classFactory packageName).
	self deny: (self packageOrganizer hasPackage: classFactory packageName , '2').
	self applyInputEntry.
	self deny: (self packageOrganizer hasPackage: classFactory packageName).
	self assert: (self packageOrganizer hasPackage: classFactory packageName , '2')
]

{ #category : 'tests' }
EpApplyTest >> testPackageRenameWithClass [
	"Let's rename a package with a class and see if the class has the right package after."

	| aClass |
	self packageOrganizer ensurePackage: classFactory packageName.

	aClass := classFactory newClass.
	self packageOrganizer renamePackage: classFactory packageName to: classFactory packageName , '2'.
	self setHeadAsInputEntry.
	self packageOrganizer renamePackage: classFactory packageName , '2' to: classFactory packageName.

	self assert: inputEntry content class equals: EpPackageRename. "Rollback"
	self assert: aClass packageName equals: classFactory packageName.
	self applyInputEntry.
	self assert: aClass packageName equals: classFactory packageName , '2'
]

{ #category : 'tests' }
EpApplyTest >> testPackageRenameWithExtension [
	"Let's rename a package with a class and see if the class has the right package after."

	| aClass |
	self packageOrganizer ensurePackage: classFactory packageName.

	aClass := classFactory make: [ :aBuilder | aBuilder package: classFactory packageName , '3' ].
	aClass compile: 'fortyTwo ^42' classified: '*' , classFactory packageName.
	self packageOrganizer renamePackage: classFactory packageName to: classFactory packageName , '2'.
	self setHeadAsInputEntry.

	self assert: (aClass >> #fortyTwo) protocolName equals: '*' , classFactory packageName , '2'.
	"Rollback"
	self packageOrganizer renamePackage: classFactory packageName , '2' to: classFactory packageName.

	self assert: inputEntry content class equals: EpPackageRename.
	self assert: (aClass >> #fortyTwo) protocolName equals: '*' , classFactory packageName.
	self applyInputEntry.
	self assert: (aClass >> #fortyTwo) protocolName equals: '*' , classFactory packageName , '2'
]

{ #category : 'tests' }
EpApplyTest >> testPackageTagAdditionWithPackageTagRemoval [

	self packageOrganizer ensureTag: classFactory tagName inPackage: classFactory packageName.
	self setHeadAsInputEntry.
	self packageOrganizer removeTag: classFactory tagName fromPackage: classFactory packageName.

	self assert: inputEntry content class equals: EpPackageTagAddition.
	self deny: (self packageOrganizer hasTag: classFactory tagName inPackage: classFactory packageName).
	self applyInputEntry.
	self assert: (self packageOrganizer hasTag: classFactory tagName inPackage: classFactory packageName)
]

{ #category : 'tests' }
EpApplyTest >> testPackageTagRemovalWithPackageTagAddition [

	self packageOrganizer ensureTag: classFactory tagName inPackage: classFactory packageName.
	self packageOrganizer removeTag: classFactory tagName fromPackage: classFactory packageName.
	self setHeadAsInputEntry.
	self packageOrganizer ensureTag: classFactory tagName inPackage: classFactory packageName.

	self assert: inputEntry content class equals: EpPackageTagRemoval.
	self assert: (self packageOrganizer hasTag: classFactory tagName inPackage: classFactory packageName).
	self applyInputEntry.
	self deny: (self packageOrganizer hasTag: classFactory tagName inPackage: classFactory packageName)
]

{ #category : 'tests' }
EpApplyTest >> testPackageTagRename [

	self packageOrganizer ensureTag: classFactory tagName inPackage: classFactory packageName.
	self packageOrganizer renameTag: classFactory tagName to: classFactory tagName , '2' inPackage: classFactory packageName.
	self setHeadAsInputEntry.

	self packageOrganizer renameTag: classFactory tagName , '2' to: classFactory tagName inPackage: classFactory packageName. "Rollback"

	self assert: inputEntry content class equals: EpPackageTagRename.
	self assert: (self packageOrganizer hasTag: classFactory tagName inPackage: classFactory packageName).
	self deny: (self packageOrganizer hasTag: classFactory tagName , '2' inPackage: classFactory packageName).
	self applyInputEntry.
	self deny: (self packageOrganizer hasTag: classFactory tagName inPackage: classFactory packageName).
	self assert: (self packageOrganizer hasTag: classFactory tagName , '2' inPackage: classFactory packageName)
]

{ #category : 'tests' }
EpApplyTest >> testPackageTagRenameWithClass [
	"Let's rename a package with a class and see if the class has the right package after."

	| aClass |
	self packageOrganizer ensureTag: classFactory tagName inPackage: classFactory packageName.

	aClass := classFactory make: [ :aBuilder | aBuilder tag: classFactory tagName ].

	self packageOrganizer renameTag: classFactory tagName to: classFactory tagName , '2' inPackage: classFactory packageName.
	self setHeadAsInputEntry.
	self packageOrganizer renameTag: classFactory tagName , '2' to: classFactory tagName inPackage: classFactory packageName.

	self assert: inputEntry content class equals: EpPackageTagRename. "Rollback"
	self assert: aClass packageName equals: classFactory packageName.
	self assert: aClass packageTagName equals: classFactory tagName.
	self applyInputEntry.
	self assert: aClass packageName equals: classFactory packageName.
	self assert: aClass packageTagName equals: classFactory tagName , '2'
]

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

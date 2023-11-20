Class {
	#name : 'EpRevertTest',
	#superclass : 'EpLogBrowserOperationFactoryTest',
	#category : 'EpiceaBrowsers-Tests-Integration',
	#package : 'EpiceaBrowsers-Tests',
	#tag : 'Integration'
}

{ #category : 'private' }
EpRevertTest >> revertInputEntry [

	EpLogBrowserOperationFactory new
		logBrowserModel: (EpLogBrowserPresenter newWithLog: monitor log);
		entries: { inputEntry };
		errorHandlerBlock: [:error | error signal ];
		revertCodeChanges
]

{ #category : 'tests' }
EpRevertTest >> testBehaviorCommentChange [

	| aClass |
	aClass := classFactory newClass.
	aClass comment: 'before'.
	aClass comment: 'after'.
	self setHeadAsInputEntry.

	self revertInputEntry.

	self assert: aClass comment equals: 'before'
]

{ #category : 'tests' }
EpRevertTest >> testBehaviorNameChange [

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	aClass rename: className, '2'.
	self setHeadAsInputEntry.

	self revertInputEntry.

	self assert: aClass name equals: className
]

{ #category : 'tests' }
EpRevertTest >> testBehaviorRepackagedChange [

	| aClass package tag newPackage newTag |
	aClass := classFactory newClass.
	package := aClass package name.
	tag := aClass packageTag name.
	newPackage := package , '2'.
	newTag := tag , '2'.
	aClass package: newPackage tag: newTag.
	self setHeadAsInputEntry.

	self assert: (inputEntry content isKindOf: EpBehaviorRepackagedChange).
	self assert: aClass package name equals: newPackage.
	self assert: aClass packageTag name equals: newTag.
	self revertInputEntry.

	self assert: aClass package name equals: package.
	self assert: aClass packageTag name equals: tag
]

{ #category : 'tests' }
EpRevertTest >> testClassAddition [

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	self setHeadAsInputEntry.

	self assert: (self class environment hasClassNamed: className).
	self revertInputEntry.
	self deny: (self class environment hasClassNamed: className)
]

{ #category : 'tests' }
EpRevertTest >> testClassAdditionWithClassAlreadyRemoved [

	| aClass className |
	aClass := classFactory newClass.
	className := aClass name.
	self setHeadAsInputEntry.

	aClass removeFromSystem.

	self deny: (self class environment hasClassNamed: className).
	self revertInputEntry.
	self deny: (self class environment hasClassNamed: className)
]

{ #category : 'tests' }
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
		description: 'The old class of the modification had no instance variables'
]

{ #category : 'tests' }
EpRevertTest >> testClassRemoval [

	| aClass className package tag |
	aClass := classFactory newClass.
	aClass classSide instanceVariableNames: 'hey'.
	className := aClass name.
	package := aClass package name.
	tag := aClass packageTag name.
	aClass removeFromSystem.
	self setHeadAsInputEntry.

	self assert: (inputEntry content isKindOf: EpClassRemoval).
	self deny: (self class environment hasClassNamed: className).
	self revertInputEntry.
	self assert: (self class environment hasClassNamed: className).
	self assert: (self class environment classNamed: className) package name equals: package.
	self assert: (self class environment classNamed: className) packageTag name equals: tag.
	self assert: (self class environment classNamed: className) classSide instVarNames equals: #( 'hey' )
]

{ #category : 'tests' }
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
	self assert: (self class environment hasClassNamed: className)
]

{ #category : 'tests' }
EpRevertTest >> testMethodAddition [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	self setHeadAsInputEntry.

	self assert: (inputEntry content isKindOf: EpMethodAddition).
	self assert: (aClass includesSelector: #fortyTwo).
	self revertInputEntry.
	self deny: (aClass includesSelector: #fortyTwo)
]

{ #category : 'tests' }
EpRevertTest >> testMethodAdditionWithMethodAlreadyRemoved [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	self setHeadAsInputEntry.

	aClass removeSelector: #fortyTwo.

	self assert: (inputEntry content isKindOf: EpMethodAddition).
	self deny: (aClass includesSelector: #fortyTwo).
	self revertInputEntry.
	self deny: (aClass includesSelector: #fortyTwo)
]

{ #category : 'tests' }
EpRevertTest >> testMethodModification [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	aClass compile: 'fortyTwo ^nil'.
	self setHeadAsInputEntry.

	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^nil'.
	self revertInputEntry.
	self assert: (aClass>>#fortyTwo) sourceCode equals: 'fortyTwo ^42'
]

{ #category : 'tests' }
EpRevertTest >> testMethodModificationWithProtocolChanged [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'one'.
	(aClass>>#fortyTwo) protocol: 'two'.
	self setHeadAsInputEntry.

	(aClass>>#fortyTwo) protocol: 'three'.

	self assert: (aClass>>#fortyTwo) protocolName equals: 'three'.
	self revertInputEntry.
	self assert: (aClass>>#fortyTwo) protocolName equals: 'one'
]

{ #category : 'tests' }
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
	self assert: (aClass>>#fortyTwo) protocolName equals: 'number'
]

{ #category : 'tests' }
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
	self assert: (aClass>>#fortyTwo) protocolName equals: 'number'
]

{ #category : 'tests' }
EpRevertTest >> testPackageTagAdditionWithPackageTagRemoval [

	self packageOrganizer ensureTag: classFactory tagName inPackage: classFactory packageName.
	self setHeadAsInputEntry.

	self assert: (self packageOrganizer hasTag: classFactory tagName inPackage: classFactory packageName).
	self revertInputEntry.
	self deny: (self packageOrganizer hasTag: classFactory tagName inPackage: classFactory packageName)
]

{ #category : 'tests' }
EpRevertTest >> testPackageTagRemovalWithPackageTagAddition [

	self packageOrganizer ensureTag: classFactory tagName inPackage: classFactory packageName.
	self packageOrganizer removeTag: classFactory tagName fromPackage: classFactory packageName.
	self setHeadAsInputEntry.

	self deny: (self packageOrganizer hasTag: classFactory tagName inPackage: classFactory packageName).
	self revertInputEntry.
	self assert: (self packageOrganizer hasTag: classFactory tagName inPackage: classFactory packageName)
]

{ #category : 'tests' }
EpRevertTest >> testPackageTagRename [

	self packageOrganizer ensureTag: classFactory tagName inPackage: classFactory packageName.
	self packageOrganizer renameTag: classFactory tagName to: classFactory tagName , '2' inPackage: classFactory packageName.
	self setHeadAsInputEntry.

	self assert: inputEntry content class equals: EpPackageTagRename.

	self deny: (self packageOrganizer hasTag: classFactory tagName inPackage: classFactory packageName).
	self assert: (self packageOrganizer hasTag: classFactory tagName , '2' inPackage: classFactory packageName).
	self revertInputEntry.
	self assert: (self packageOrganizer hasTag: classFactory tagName inPackage: classFactory packageName).
	self deny: (self packageOrganizer hasTag: classFactory tagName , '2' inPackage: classFactory packageName)
]

{ #category : 'tests' }
EpRevertTest >> testProtocolAddition [

	| aClass |
	aClass := classFactory newClass.
	aClass addProtocol: 'protocol'.
	self setHeadAsInputEntry.

	self assert: inputEntry content class equals: EpProtocolAddition.
	self assert: (aClass hasProtocol: 'protocol').
	self revertInputEntry.
	self deny: (aClass hasProtocol: 'protocol')
]

{ #category : 'tests' }
EpRevertTest >> testProtocolRemoval [

	| aClass |
	aClass := classFactory newClass.
	aClass addProtocol: 'protocol'.
	aClass removeProtocol: 'protocol'.
	self setHeadAsInputEntry.

	self assert: inputEntry content class equals: EpProtocolRemoval.
	self deny: (aClass hasProtocol: 'protocol').
	self revertInputEntry.
	self assert: (aClass hasProtocol: 'protocol')
]

{ #category : 'tests' }
EpRevertTest >> testTraitAddition [

	| aTrait traitName |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	self setHeadAsInputEntry.

	self assert: (self class environment includesKey: traitName).
	self revertInputEntry.
	self deny: (self class environment includesKey: traitName)
]

{ #category : 'tests' }
EpRevertTest >> testTraitAdditionWithTraitAlreadyRemoved [

	| aTrait traitName |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	self setHeadAsInputEntry.

	aTrait removeFromSystem.

	self deny: (self class environment includesKey: traitName).
	self revertInputEntry.
	self deny: (self class environment includesKey: traitName)
]

{ #category : 'tests' }
EpRevertTest >> testTraitRemoval [

	| aTrait traitName |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	aTrait removeFromSystem.
	self setHeadAsInputEntry.

	self deny: (self class environment includesKey: traitName).
	self revertInputEntry.
	self assert: (self class environment includesKey: traitName)
]

{ #category : 'tests' }
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
	self assert: (self class environment includesKey: traitName)
]

Class {
	#name : #EpApplyPreviewerTest,
	#superclass : #EpLogBrowserOperationFactoryTest,
	#category : #'EpiceaBrowsersTests-Integration'
}

{ #category : #resources }
EpApplyPreviewerTest >> assertEmptyPreviewLog [

	self assert: self outputEvents isEmpty description: 'Shouldn''t find anything to apply'.
]

{ #category : #resources }
EpApplyPreviewerTest >> assertOutputsAnEventWith: aBlock [

	| outputEvents |
	outputEvents := self outputEvents.

	self assert: outputEvents size equals: 1.
	
	aBlock value: outputEvents anyOne.

]

{ #category : #resources }
EpApplyPreviewerTest >> assertOutputsEventsWith: aBlock [

	| logBrowserModel outputLog |
	logBrowserModel := EpLogBrowserModel newWithLog: monitor log.
	
	outputLog :=
		EpLogBrowserOperationFactory new
			logBrowserModel: logBrowserModel;
			entries: inputEntries;
			errorHandlerBlock: [:error | error signal ];
			newApplyPreviewLog.
	
	aBlock value: (outputLog entries collect: #content).

]

{ #category : #resources }
EpApplyPreviewerTest >> outputEvents [

	| outputLog logBrowserModel |
	logBrowserModel := EpLogBrowserModel newWithLog: monitor log.
	
	outputLog :=
		EpLogBrowserOperationFactory new
			logBrowserModel: logBrowserModel;
			entries: inputEntries;
			errorHandlerBlock: [:error | error signal ];
			newApplyPreviewLog.
	
	^ outputLog entries collect: #content
]

{ #category : #tests }
EpApplyPreviewerTest >> testBehaviorCategoryChange [

	| aClass aCategory anotherCategory |
	aClass := classFactory newClass.
	aCategory := aClass category.
	anotherCategory := aCategory, '2'.
	aClass category: anotherCategory.
	self setHeadAsInputEntry.

	aClass category: aCategory.
	
	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpBehaviorCategoryChange.
		self assert: output oldCategory equals: aCategory.
		self assert: output newCategory equals: anotherCategory.
		]

]

{ #category : #tests }
EpApplyPreviewerTest >> testBehaviorCommentChange [

	| aClass |
	aClass := classFactory newClass.
	aClass classComment: 'before'.
	self setHeadAsInputEntry.
	aClass classComment: 'after'.

	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpBehaviorCommentChange.
		self assert: output oldComment equals: 'after'.
		self assert: output newComment equals: 'before'.
		]

]

{ #category : #tests }
EpApplyPreviewerTest >> testBehaviorNameChange [

	| aClass oldName newName |
	aClass := classFactory newClass.
	oldName := aClass name.
	newName := oldName, '2'.
	aClass rename: newName.
	self setHeadAsInputEntry.

	aClass rename: oldName. "Rollback"

	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpBehaviorNameChange.
		self assert: output oldName equals: oldName.
		self assert: output newName equals: newName.
		]

]

{ #category : #tests }
EpApplyPreviewerTest >> testCategoryAdditionWithCategoryRemoved [

	self class environment organization addCategory: classFactory defaultCategory.
	self setHeadAsInputEntry.
	self class environment organization removeCategory: classFactory defaultCategory.

	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpCategoryAddition.
		self assert: output categoryName equals: classFactory defaultCategory.
		]
]

{ #category : #tests }
EpApplyPreviewerTest >> testCategoryRemovalWithCategoryAdded [

	self class environment organization addCategory: classFactory defaultCategory.
	self class environment organization removeCategory: classFactory defaultCategory.
	self setHeadAsInputEntry.

	self class environment organization addCategory: classFactory defaultCategory.

	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpCategoryRemoval.
		self assert: output categoryName equals: classFactory defaultCategory.
		]
]

{ #category : #tests }
EpApplyPreviewerTest >> testCategoryRenameWithPreviousRollback [

	| organization aCategory anotherCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	anotherCategory := aCategory, '2'.
	organization addCategory: aCategory.
	organization renameCategory: aCategory toBe: anotherCategory.
	self setHeadAsInputEntry.
	organization renameCategory: anotherCategory toBe: aCategory. "Rollback"

	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpCategoryRename.
		self assert: output oldCategoryName equals: aCategory.
		self assert: output newCategoryName equals: anotherCategory.
		]

]

{ #category : #tests }
EpApplyPreviewerTest >> testClassAdditionWithClassRemoved [

	| aClass className classDefinition |
	aClass := classFactory newClass.
	className := aClass name.
	classDefinition := aClass definition.
	self setHeadAsInputEntry.

	aClass removeFromSystem.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpClassAddition.
		self assert: output behaviorAffected definitionSource equals: classDefinition.
		]
]

{ #category : #tests }
EpApplyPreviewerTest >> testClassAdditionWithInstanceVariablesChanged [

	| aClass classDefinition |
	aClass := classFactory newClass.
	classDefinition := aClass definition.
	self setHeadAsInputEntry.
	aClass addInstVarNamed: #x.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpClassModification.
		self assert: output oldClass definitionSource equals: aClass definition.
		self assert: output newClass definitionSource equals: classDefinition.
		]

]

{ #category : #tests }
EpApplyPreviewerTest >> testClassAdditionWithSuperclassChanged [

	| aClass classDefinition |
	aClass := classFactory newClass.
	classDefinition := aClass definition.
	self setHeadAsInputEntry.
	aClass superclass: Array.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpClassModification.
		self assert: output oldClass definitionSource equals: aClass definition.
		self assert: output newClass definitionSource equals: classDefinition.
		].

	aClass superclass: Object. "Restore original superclass else something in hierarchy stays wrong (and ClassHierarchyTest>>testSubclassInstVar fails)"

]

{ #category : #tests }
EpApplyPreviewerTest >> testClassRemovalWithClassAdded [

	| aClass className classDefinition |
	aClass := classFactory newClass.
	className := aClass name.
	classDefinition := aClass definition.
	aClass removeFromSystem.
	
	self setHeadAsInputEntry.
	
	aClass := classFactory newClass.
	aClass rename: className.
	
	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpClassRemoval.
		self assert: output behaviorAffected definitionSource equals: aClass definition.
		]

]

{ #category : #tests }
EpApplyPreviewerTest >> testKeepChangeWhenMethodAdditionWithAbsentBehavior [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.
	aClass removeFromSystem.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpMethodAddition ]

]

{ #category : #tests }
EpApplyPreviewerTest >> testKeepChangeWhenMethodModificationWithAbsentBehavior [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass compile: 'fortyTwo ^nil' classified: 'number'.
	self setHeadAsInputEntry.
	aClass removeFromSystem.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpMethodModification ]

]

{ #category : #tests }
EpApplyPreviewerTest >> testMethodAdditionWithMethodRemoved [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.
	aClass removeSelector: #fortyTwo.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpMethodAddition.
		self assert: output methodAffectedSourceCode equals: 'fortyTwo ^42'.
		self assert: output methodAffectedProtocol equals: 'number'.
		]
]

{ #category : #tests }
EpApplyPreviewerTest >> testMethodAdditionWithProtocolChanged [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.
	(aClass>>#fortyTwo) protocol: 'none'.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpMethodModification.
		self assert: output oldSourceCode equals: 'fortyTwo ^42'.
		self assert: output oldProtocol equals: 'none'.
		self assert: output newSourceCode equals: 'fortyTwo ^42'.
		self assert: output newProtocol equals: 'number'.
		]
]

{ #category : #tests }
EpApplyPreviewerTest >> testMethodAdditionWithSourceCodeChanged [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.
	aClass compile: 'fortyTwo ^nil'.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpMethodModification.
		self assert: output oldSourceCode equals: 'fortyTwo ^nil'.
		self assert: output oldProtocol equals: 'number'.
		self assert: output newSourceCode equals: 'fortyTwo ^42'.
		self assert: output newProtocol equals: 'number'.
		]
]

{ #category : #tests }
EpApplyPreviewerTest >> testMethodModificationWithMethodRemoved [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass compile: 'fortyTwo ^nil' classified: 'number'.
	self setHeadAsInputEntry.
	aClass removeSelector: #fortyTwo.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpMethodAddition.
		self assert: output methodAffectedSourceCode equals: 'fortyTwo ^nil'.
		self assert: output methodAffectedProtocol equals: 'number'.
		]
]

{ #category : #tests }
EpApplyPreviewerTest >> testMethodRemovalWithMethodAdded [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass removeSelector: #fortyTwo.
	self setHeadAsInputEntry.
	aClass compile: 'fortyTwo ^42' classified: 'number'.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpMethodRemoval.
		self assert: output methodAffectedSourceCode equals: 'fortyTwo ^42'.
		self assert: output methodAffectedProtocol equals: 'number'.
		]
]

{ #category : #tests }
EpApplyPreviewerTest >> testNonCodeChangeEvent [

	monitor logEntryCommented:
		(EpLogEntriesComment 
			entryReferences: #()
			oldComment: ''
			newComment: '42').

	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.

]

{ #category : #tests }
EpApplyPreviewerTest >> testProtocolAdditionWithProtocolRemoved [

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	self setHeadAsInputEntry.
	aClass removeProtocol: 'protocol'.

	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpProtocolAddition.
		self assert: output protocol equals: 'protocol'.
		]

]

{ #category : #tests }
EpApplyPreviewerTest >> testProtocolRemovalWithProtocolAdded [

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	aClass removeProtocol: 'protocol'.
	self setHeadAsInputEntry.
	aClass organization addCategory: 'protocol'.

	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpProtocolRemoval.
		self assert: output protocol equals: 'protocol'.
		]

]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantBehaviorCategoryChange [

	| aClass aCategory anotherCategory |
	aClass := classFactory newClass.
	aCategory := aClass category.
	anotherCategory := aCategory, '2'.
	aClass category: anotherCategory.
	aClass category: aCategory. "Rollback"
	self setHeadAsInputEntry.
	
	self assertEmptyPreviewLog.
]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantBehaviorCategoryChangeWithAbsentBehavior [

	| aClass aCategory anotherCategory |
	aClass := classFactory newClass.
	aCategory := aClass category.
	anotherCategory := aCategory, '2'.
	aClass category: anotherCategory.
	aClass category: aCategory. "Rollback"
	self setHeadAsInputEntry.
	aClass removeFromSystem.

	self assertEmptyPreviewLog.
]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantBehaviorCommentChange [

	| aClass |
	aClass := classFactory newClass.
	aClass classComment: 'before'.
	aClass classComment: 'after'.
	aClass classComment: 'before'.
	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.
]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantBehaviorCommentChangeWithAbsentBehavior [

	| aClass |
	aClass := classFactory newClass.
	aClass classComment: 'before'.
	self setHeadAsInputEntry.
	aClass removeFromSystem.

	self assertEmptyPreviewLog.
]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantCategoryAddition [

	self class environment organization addCategory: classFactory defaultCategory.
	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.
]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantCategoryRemoval [

	self class environment organization addCategory: classFactory defaultCategory.
	self class environment organization removeCategory: classFactory defaultCategory.
	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.
]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantCategoryRenameWithAbsentCategory [

	| organization aCategory anotherCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	anotherCategory := aCategory, '2'.
	organization addCategory: aCategory.
	organization renameCategory: aCategory toBe: anotherCategory.
	self setHeadAsInputEntry.

	self assertEmptyPreviewLog
]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantClassAddition [

	| aClass |
	aClass := classFactory newClass.
	self setHeadAsInputEntry.
	
	self assertEmptyPreviewLog.
]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantClassRemoval [

	| aClass |
	aClass := classFactory newClass.
	aClass removeFromSystem.
	self setHeadAsInputEntry.
	
	self assertEmptyPreviewLog.
]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantMethodAddition [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.

]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantMethodModification [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass compile: 'fortyTwo ^nil' classified: 'number'.
	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.

]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantMethodRemoval [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass removeSelector: #fortyTwo.
	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.
]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantMethodRemovalWithAbsentBehavior [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass removeSelector: #fortyTwo.
	self setHeadAsInputEntry.
	aClass removeFromSystem.

	self assertEmptyPreviewLog.
]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantProtocolAddition [

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.
]

{ #category : #tests }
EpApplyPreviewerTest >> testRedundantProtocolRemoval [

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	aClass removeProtocol: 'protocol'.
	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.
]

{ #category : #'tests - multiple changes' }
EpApplyPreviewerTest >> testRedundantSequenceOfMethodModifications [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass compile: 'fortyTwo ^43' classified: 'number'.
	aClass compile: 'fortyTwo ^44' classified: 'number'.
	self setMonitorLogAsInputEntries.

	self assertEmptyPreviewLog
]

{ #category : #'tests - multiple changes' }
EpApplyPreviewerTest >> testSequenceOfClassAndMethodAddition [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	self setMonitorLogAsInputEntries.

	aClass removeSelector: #fortyTwo.
	aClass removeFromSystem.

	self assertOutputsEventsWith: [:output | 
		| outputClasses |
		outputClasses := output collect: #class as: Array.
		self assert: outputClasses equals: { EpClassAddition. EpMethodAddition }.
		]

]

{ #category : #'tests - multiple changes' }
EpApplyPreviewerTest >> testSequenceOfMethodModifications [

	| aClass |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	aClass compile: 'fortyTwo ^43' classified: 'number'.
	aClass compile: 'fortyTwo ^44' classified: 'number'.
	self setMonitorLogAsInputEntries.

	aClass removeSelector: #fortyTwo.
	aClass removeFromSystem.

	self assertOutputsEventsWith: [:output | 
		| outputClasses |
		outputClasses := output collect: #class as: Array.
		self assert: outputClasses equals: { EpClassAddition. EpMethodModification }.
		self assert: output last methodAffectedSourceCode equals: 'fortyTwo ^44'.
		]

]

{ #category : #tests }
EpApplyPreviewerTest >> testTraitAdditionWithTraitRemoved [

	| aTrait traitDefinition |
	aTrait := classFactory newTrait.
	traitDefinition := aTrait definition.
	self setHeadAsInputEntry.

	aTrait removeFromSystem.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpTraitAddition.
		self assert: output behaviorAffected definitionSource equals: traitDefinition.
		]
]

{ #category : #tests }
EpApplyPreviewerTest >> testTraitModification [
	<expectedFailure>
	
	self fail. "Missing test/feature"
]

{ #category : #tests }
EpApplyPreviewerTest >> testTraitRemovalWithTraitAdded [

	| aTrait traitName traitDefinition |
	aTrait := classFactory newTrait.
	traitName := aTrait name.
	traitDefinition := aTrait definition.
	aTrait removeFromSystem.
	self setHeadAsInputEntry.

	aTrait := classFactory newTrait.
	aTrait rename: traitName.

	self assertOutputsAnEventWith: [:output | 
		self assert: output class equals: EpTraitRemoval.
		self assert: output behaviorAffected definitionSource equals: traitDefinition.
		]

]

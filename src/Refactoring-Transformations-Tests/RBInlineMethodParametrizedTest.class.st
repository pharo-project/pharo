Class {
	#name : 'RBInlineMethodParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBInlineMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBInlineMethodRefactoring };
		yourself
]

{ #category : 'accessing' }
RBInlineMethodParametrizedTest >> constructor [
	^ #inline:inMethod:forClass:
]

{ #category : 'mocking' }
RBInlineMethodParametrizedTest >> rbModelForInliningMethodTest [

	| newModel |
	newModel := self modelWithoutRealClasses:
		            #( #RBRenameClassVariableChange #RBVariableCompositeRefactoryChange #RBRenameInstanceVariableChange #RBCompositeRefactoryChange
		               #RBRenameVariableChange #RBRefactoryChange ).
	newModel
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #RBRefactoryChange;
				slots: { #name };
				package: 'Refactory-Support' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #RBRefactoryChange;
				name: #RBCompositeRefactoryChange;
				slots: { #changes };
				package: 'Refactory-Support' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #RBCompositeRefactoryChange;
				name: #RBVariableCompositeRefactoryChange;
				slots: { #className. #isMeta };
				package: 'Refactory-Support' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #RBVariableCompositeRefactoryChange;
				name: #RBRenameVariableChange;
				slots: { #oldName. #newName };
				package: 'Refactory-Support' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #RBRenameVariableChange;
				name: #RBRenameClassVariableChange;
				package: 'Refactory-Support' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #RBRefactoryChange;
				name: #RBCompositeRefactoryChange;
				slots: { #changes };
				package: 'Refactory-Support' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #RBRenameVariableChange;
				name: #RBRenameInstanceVariableChange;
				package: 'Refactory-Support' ].


	self
		addMethodsToModelClasses: #( #( #RBRenameClassVariableChange #( #( 'removeOldVariable
					(RBRemoveClassVariableChange remove: oldName from: self changeClass) execute' #private ) #( 'addNewVariable
					(RBAddClassVariableChange add: newName to: self changeClass) execute' #private ) #( 'executeNotifying: aBlock
	| undo |
	self addNewVariable.
	self copyOldValuesToNewVariable.
	undo := super executeNotifying: aBlock.
	self removeOldVariable.
	^undo' #private ) #( 'copyOldValuesToNewVariable
	| oldValue |
	oldValue := self changeClass classPool at: oldName ifAbsent: [].
	self changeClass at: newName asSymbol put: oldValue' #private ) ) ) #( #RBVariableCompositeRefactoryChange #( #( 'displayClassName
	^isMeta
		ifTrue: [self changeClassName , '' class'']
		ifFalse: [self changeClassName asString]' #printing ) #( 'isMeta
	^isMeta' #private ) #( 'changeClass: aBehavior
	isMeta := aBehavior isMeta.
	className := isMeta
				ifTrue: [aBehavior soleInstance name]
				ifFalse: [aBehavior name]' #accessing ) #( 'hash
	^self changeClassName hash' #comparing ) #( 'changeClassName: aSymbol
	className := aSymbol.
	isMeta isNil ifTrue: [isMeta := false]' #accessing ) #( 'changeClass
	| class |
	class := Smalltalk at: self changeClassName ifAbsent: [^nil].
	^isMeta ifTrue: [class class] ifFalse: [class]' #accessing ) #( 'printOn: aStream
	aStream nextPutAll: self displayString' #printing ) #( 'changeClassName
	^className' #accessing ) #( '= aRefactoryClassChange
	self class = aRefactoryClassChange class ifFalse: [^false].
	^className = aRefactoryClassChange changeClassName
		and: [isMeta = aRefactoryClassChange isMeta]' #comparing ) ) ) #( #RBRenameInstanceVariableChange #( #( 'removeOldVariable
	(RBRemoveInstanceVariableChange remove: oldName from: self changeClass)
		execute' #private ) #( 'addNewVariable
	(RBAddInstanceVariableChange add: newName to: self changeClass) execute' #private ) #( 'executeNotifying: aBlock
	| undo |
	self addNewVariable.
	self copyOldValuesToNewVariable.
	undo := super executeNotifying: aBlock.
	self removeOldVariable.
	^undo' #private ) #( 'copyOldValuesToNewVariable
	| newIndex oldIndex |
	oldIndex := self changeClass allInstVarNames indexOf: oldName asString.
	newIndex := self changeClass allInstVarNames indexOf: newName asString.
	self changeClass withAllSubclasses do:
			[:each |
			each allInstances
				do: [:inst | each instVarAt: newIndex put: (each instVarAt: oldIndex)]]' #private ) ) ) #( #RBCompositeRefactoryChange #( #( 'compile: source in: class
			^self addChange: (RBAddMethodChange compile: source in: class)' #'refactory-changes' ) #( 'addClassVariable: variableName to: aClass
	^self addChange: (RBAddClassVariableChange add: variableName to: aClass)' #'refactory-changes' ) #( 'addChangeFirst: aRefactoryChange
	changes addFirst: aRefactoryChange.
	^aRefactoryChange' #accessing ) #( 'removeChange: aChange
	changes remove: aChange ifAbsent: []' #'private-inspector accessing' ) #( 'addPool: aPoolVariable to: aClass
	^self addChange: (RBAddPoolVariableChange add: aPoolVariable to: aClass)' #'refactory-changes' ) #( 'initialize
	super initialize.
	changes := OrderedCollection new' #'initialize-release' ) #( 'defineClass: aString
	^self addChange: (RBAddClassChange definition: aString)' #'refactory-changes' ) #( 'changeForClass: aRBClass selector: aSelector
	changes reverseDo:
			[:each |
			| change |
			change := each changeForClass: aRBClass selector: aSelector.
			change notNil ifTrue: [^change]].
	^nil' #accessing ) #( 'removeInstanceVariable: variableName from: aClass
	^self
		addChange: (RBRemoveInstanceVariableChange remove: variableName from: aClass)' #'refactory-changes' ) #( 'printOn: aStream
	aStream nextPutAll: name' #printing ) #( 'inspect
	RefactoryBuilderInspector openOn: self' #'user interface' ) #( 'flattenOnto: aCollection
	changes do: [:each | each flattenOnto: aCollection]' #private ) #( 'hash
	^changes size' #comparing ) #( '= aRefactoryBuilder
	self class = aRefactoryBuilder class ifFalse: [^false].
	changes size = aRefactoryBuilder changes size ifFalse: [^false].
	changes with: aRefactoryBuilder changes
		do: [:each :change | each = change ifFalse: [^false]].
	^true' #comparing ) #( 'renameClass: class to: newName
	^self addChange: (RenameClassChange rename: class name to: newName)' #'refactory-changes' ) #( 'renameChangesForClass: aClassName to: newClassName
	^(self copy)
		changes: (self changes
					collect: [:each | each renameChangesForClass: aClassName to: newClassName]);
		yourself' #accessing ) #( 'postCopy
	super postCopy.
	changes := changes collect: [:each | each copy]' #copying ) #( 'changes: aCollection
	changes := aCollection' #'private-inspector accessing' ) #( 'addInstanceVariable: variableName to: aClass
	^self addChange: (RBAddInstanceVariableChange add: variableName to: aClass)' #'refactory-changes' ) #( 'compile: source in: class classified: aProtocol
	^self addChange: (RBAddMethodChange
				compile: source
				in: class
				classified: aProtocol)' #'refactory-changes' ) #( 'changeForMetaclass: aSymbol selector: aSelector
	changes reverseDo:
			[:each |
			| change |
			change := each changeForMetaclass: aSymbol selector: aSelector.
			change notNil ifTrue: [^change]].
	^nil' #accessing ) #( 'removeClassVariable: variableName from: aClass
	^self
		addChange: (RBRemoveClassVariableChange remove: variableName from: aClass)' #'refactory-changes' ) #( 'executeNotifying: aBlock
	| undos undo |
	undos := changes collect: [:each | each executeNotifying: aBlock].
	undo := self copy.
	undo changes: undos reverse.
	^undo' #private ) #( 'changes
	^changes' #'private-inspector accessing' ) #( 'removeMethod: aSelector from: aClass
	^self addChange: (RBRemoveMethodChange remove: aSelector from: aClass)' #'refactory-changes' ) #( 'removeClass: aClass
	^self addChange: (RBRemoveClassChange removeClassName: aClass)' #'refactory-changes' ) #( 'addChange: aRefactoryChange
	changes add: aRefactoryChange.
	^aRefactoryChange' #accessing ) #( 'changesSize
	^changes inject: 0 into: [:sum :each | sum + each changesSize]' #accessing ) #( 'displayString
	^super displayString asText allBold' #printing ) #( 'problemCount
	^self changesSize' #accessing ) ) ) #( #RBRenameVariableChange #( #( 'oldName: aString
	oldName := aString' #private ) #( 'executeNotifying: aBlock
	| undo |
	undo := super executeNotifying: aBlock.
	undo
		oldName: newName;
		newName: oldName.
	^undo' #private ) #( 'newName: aString
	newName := aString' #private ) #( 'changeString
	^''Rename '' , oldName , '' to '' , newName' #printing ) ) ) #( #RBRefactoryChange #( #( 'flattenedChanges
	| changes |
	changes := OrderedCollection new.
	self flattenOnto: changes.
	^changes' #private ) #( 'name: aString
	name := aString' #'initialize-release' ) #( 'initialize' #'initialize-release' ) #( 'changeForMetaclass: aSymbol selector: aSelector
	^nil' #accessing ) #( 'changeString
	^self class name' #printing ) #( 'changeForClass: aRBClass selector: aSelector
	^nil' #accessing ) #( 'executeWithMessage: aString
	| tally controller m done |
	m := 0 asValue.
	done := 0.
	tally := self changesSize.
	controller := aString isNil
				ifTrue: [nil]
				ifFalse: [ProgressWidgetView progressOpenOn: m label: aString].
	m value: 0.
	^
	[self executeNotifying:
			[done := done + 1.
			m value: done asFloat / tally]]
			ensure: [controller notNil ifTrue: [controller closeAndUnschedule]]' #'performing-changes' ) #( 'executeNotifying: aBlock
	self subclassResponsibility' #private ) #( 'changes
	^Array with: self' #accessing ) #( 'execute
	^self executeNotifying: []' #'performing-changes' ) #( 'inspect
	^((RBCompositeRefactoryChange new)
		changes: (Array with: self);
		yourself) inspect' #'user interface' ) #( 'flattenOnto: aCollection
	aCollection add: self' #private ) #( 'name
	^name isNil ifTrue: [self changeString] ifFalse: [name]' #accessing ) #( 'changesSize
	^1' #accessing ) #( 'displayString
	^name isNil ifTrue: [self changeString] ifFalse: [name]' #printing ) #( 'renameChangesForClass: aClassName to: newClassName
	"We are in the middle of performing a rename operation. If we stored
	the class name, we need to change the class name to the new
	name to perform the compiles."

	self subclassResponsibility' #accessing ) ) ) )
		in: newModel.

	^ newModel
]

{ #category : 'failure tests' }
RBInlineMethodParametrizedTest >> testFailureBadInterval [
	self shouldFail:
		(self createRefactoringWithArguments: { (13 to: 23) . #testMethod . RBClassDataForRefactoringTest }).
	self shouldFail:
		(self createRefactoringWithArguments: { (14 to: 17) . #testMethod . RBClassDataForRefactoringTest }).
	self shouldFail:
		(self createRefactoringWithArguments: { (24 to: 30) . #testMethod . RBClassDataForRefactoringTest }).
	self shouldFail:
		(self createRefactoringWithArguments: { (1 to: 30) . #testMethod . RBClassDataForRefactoringTest })
]

{ #category : 'failure tests' }
RBInlineMethodParametrizedTest >> testFailureInlineMethodForSuperSendThatAlsoSendsSuper [

	| refactoring |
	model := self rbModelForInliningMethodTest.
	refactoring := self createRefactoringWithArguments: {
			               (102 to: 131).
			               #executeNotifying:.
			               #RBRenameInstanceVariableChange }.
	self shouldFail: refactoring
]

{ #category : 'failure tests' }
RBInlineMethodParametrizedTest >> testFailureNonExistantSelector [
	self shouldFail: (self createRefactoringWithArguments:
		{ (14 to: 17) . #checkClass1: . RBClassDataForRefactoringTest })
]

{ #category : 'failure tests' }
RBInlineMethodParametrizedTest >> testFailureOverriden [

	self shouldWarn: (self createRefactoringWithArguments: {
				 (15 to: 26).
				 #failedRules.
				 RBLintRuleTestData })
]

{ #category : 'failure tests' }
RBInlineMethodParametrizedTest >> testFailurePrimitive [

	self shouldFail: (self createRefactoringWithArguments: {
				 (14 to: 23).
				 #testMethod.
				 RBClassDataForRefactoringTest })
]

{ #category : 'failure tests' }
RBInlineMethodParametrizedTest >> testFailureReturn [

	self shouldFail: (self createRefactoringWithArguments: {
				 (418 to: 485).
				 #utilityMethods.
				 RBBasicLintRuleTestData class })
]

{ #category : 'tests' }
RBInlineMethodParametrizedTest >> testInlineMethod [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ (451 to: 500) .#sentNotImplementedInApplication . RBBasicLintRuleTestData class }.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model metaclassNamed: #RBBasicLintRuleTestData) parseTreeForSelector: #sentNotImplementedInApplication) equals: (self parseMethod: 'sentNotImplementedInApplication
									| detector |
									detector := self new.
									detector name: ''Messages sent but not implemented in application''.
									detector methodBlock:
											[:context :result |
											| message class block |
											message := context messages
														detect: [:each | (context isItem: each in: context application) not]
														ifNone: [nil].
											class := context selectedClass.
											block :=
													[:each |
													| app methodApp root |
													app := context application.
													((class canUnderstand: each)
														ifTrue:
															[root := app rootApplication.
															methodApp := ((class whichClassIncludesSelector: each)
																		compiledMethodAt: each) application
																		rootApplication.
															methodApp == root or: [root isBasedOn: methodApp]]
														ifFalse: [false]) not].
											message isNil
												ifTrue: [message := context selfMessages detect: block ifNone: [nil]].
											message isNil
												ifTrue:
													[class := class superclass.
													class isNil
														ifTrue:
															[context superMessages isEmpty
																ifFalse: [message := context superMessages asArray first]]
														ifFalse: [message := context superMessages detect: block ifNone: [nil]]].
											message isNotNil
												ifTrue:
													[result addSearchString: message.
													result addClass: context selectedClass selector: context selector]].
									^detector')
]

{ #category : 'tests' }
RBInlineMethodParametrizedTest >> testInlineMethod1 [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ (39 to: 84) . #caller . RBClassDataForRefactoringTest }.
	self setupInlineExpressionFor: refactoring toReturn: false.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #caller) equals: (self parseMethod: 'caller

	| anObject anObject1 |
	anObject := 5.
	anObject1 := anObject + 1.
	anObject1 printString
		traceCr;
		cr.
	^ anObject')
]

{ #category : 'tests' }
RBInlineMethodParametrizedTest >> testInlineMethod2 [
	| refactoring methodName |
	methodName := #caller1.
	refactoring := self createRefactoringWithArguments:
		{ (40 to: 120) .  methodName . RBClassDataForRefactoringTest }.
	self setupInlineExpressionFor: refactoring toReturn: false.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: methodName) equals: (self parseMethod: 'caller1

	| anObject each1 anObject1 |
	anObject := 5.
	anObject1 := anObject + 1.
	each1 := anObject1 printString.
	each1 traceCr.
	[ :each |
	each printString.
	^ anObject ] value: each1')
]

{ #category : 'tests' }
RBInlineMethodParametrizedTest >> testInlineMethod3 [
	| refactoring methodName |
	methodName := #caller2.
	refactoring := self createRefactoringWithArguments:
		{ (58 to: 73) . methodName . RBClassDataForRefactoringTest }.
	self setupInlineExpressionFor: refactoring toReturn: false.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: methodName) equals: (self parseMethod: 'caller2
								^(1 to: 10) inject: 1 into: [:sum :each | sum * ((1 to: 10) inject: each into: [:sum1 :each1 | sum1 + each1])]	')
]

{ #category : 'tests' }
RBInlineMethodParametrizedTest >> testInlineMethod4 [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ (31 to: 112) . #inlineJunk . RBClassDataForRefactoringTest }.
	self setupInlineExpressionFor: refactoring toReturn: false.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #inlineJunk) equals: (self parseMethod: 'inlineJunk
										| asdf bar1 baz1 asdf1 |
										bar1 :=
												[:each |
												| temp |
												temp := each.
												temp , temp] value: self.
										baz1 := bar1 + bar1.
										asdf1 := baz1 + bar1.
										asdf := asdf1.
										^asdf foo:
												[:bar |
												| baz |
												baz := bar.
												baz * baz]')
]

{ #category : 'tests' }
RBInlineMethodParametrizedTest >> testInlineMethod5 [
	| refactoring |

	refactoring := self createRefactoringWithArguments:
		{ (53 to: 64) . #inlineLast . RBClassDataForRefactoringTest }.
	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #inlineLast) equals: (self parseMethod: 'inlineLast
									5 = 3 ifTrue: [^self caller] ifFalse: [^	(1 to: 10) inject: 1 into: [:sum :each | sum * (self foo: each)]]')
]

{ #category : 'tests' }
RBInlineMethodParametrizedTest >> testInlineMethodForSuperSend [
	| refactoring |
	model := self rbModelForInliningMethodTest.
	(model classNamed: #RBRenameVariableChange)
		removeMethod: #executeNotifying:.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ (101 to: 130) . #executeNotifying: . (model classNamed: #RBRenameInstanceVariableChange) }.
	self executeRefactoring: refactoring.

	self
		assert: ((model classNamed: #RBRenameInstanceVariableChange)
				parseTreeForSelector: #executeNotifying:) equals: (self
							parseMethod: 'executeNotifying: aBlock
									| undo undos undo1 |
									self addNewVariable.
									self copyOldValuesToNewVariable.
									undos := changes collect: [:each | each executeNotifying: aBlock].
									undo1 := self copy.
									undo1 changes: undos reverse.
									undo := undo1.
									self removeOldVariable.
									^undo')
]

{ #category : 'tests' }
RBInlineMethodParametrizedTest >> testInlineRecursiveCascadedMethod [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ (33 to: 62) . #inlineMethod . RBClassDataForRefactoringTest }.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #inlineMethod) equals: (self parseMethod: 'inlineMethod
									| temp temp1 |
									self foo.
									temp1 := self foo; inlineMethod; bar.
									temp := self bar.
									^temp')
]

{ #category : 'tests' }
RBInlineMethodParametrizedTest >> testModelInlineRecursiveMethod [
	| refactoring class |
	class := model classNamed: #Object.
	class compile: 'foo self bar. self foo. self bar' classified: #(#accessing).
	refactoring := self createRefactoringWithModel: model andArguments:
		{ (15 to: 23) . #foo . class }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	self assert: (class parseTreeForSelector: #foo)
		equals: (self parseMethod: 'foo self bar. self bar. self foo. self bar. self bar')
]

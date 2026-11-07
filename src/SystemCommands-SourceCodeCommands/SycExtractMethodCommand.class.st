"
I am a command to extract selected ast node into separate method 
"
Class {
	#name : 'SycExtractMethodCommand',
	#superclass : 'SycSourceCodeRefactoringCommand',
	#instVars : [
		'selectedTextInterval'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycExtractMethodCommand class >> canBeExecutedInContext: aSourceCodeContext [

	^ (super canBeExecutedInContext: aSourceCodeContext) and: [
		  aSourceCodeContext isMethodSelected not ]
]

{ #category : 'testing' }
SycExtractMethodCommand class >> methodEditorShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $e meta, $m meta for: ClySourceCodeContext
]

{ #category : 'converting' }
SycExtractMethodCommand >> asRefactorings [

	| selectedInterval refactoring |
	selectedInterval := (self selectedTextInterval ifNil: [(1 to: 0)]) ifEmpty: [
		                    self sourceNode sourceInterval ].
	refactoring := RBExtractMethodRefactoring
		               extract: selectedInterval
		               from: self method selector
		               in: self method origin.
	self setUpOptionsOf: refactoring.
	^ { refactoring }
]

{ #category : 'accessing' }
SycExtractMethodCommand >> description [

	^ 'Extracts selected ast node into separate method.'
]

{ #category : 'accessing' }
SycExtractMethodCommand >> iconName [
	^ #restart
]

{ #category : 'accessing' }
SycExtractMethodCommand >> name [
	^ '(Old refactor) Extract method (no occurrences)'
]

{ #category : 'accessing' }
SycExtractMethodCommand >> order [

	^ 1.4
]

{ #category : 'execution' }
SycExtractMethodCommand >> prepareFullExecution [

	super prepareFullExecution.
	self setUpModelFromContext: self context.
]

{ #category : 'accessing' }
SycExtractMethodCommand >> selectedTextInterval [
	^ selectedTextInterval ifNil: [ self context ifNotNil: [ :ctx | ctx selectedTextInterval ] ]
]

{ #category : 'execution' }
SycExtractMethodCommand >> setUpOptionToChangeExtractionClass: refactoring [

	refactoring setOption: #extractionClass toUse: [ :ref | | cls superclasses |
		cls := ref methodClass realClass.
		superclasses := cls allSuperclasses reversed copyWithoutAll: { Object . ProtoObject }.
		superclasses ifNotEmpty: [
			cls := self morphicUIManager
				chooseFrom: ((superclasses, {cls}) collect: #name)
				values: superclasses, {cls}
				title: 'Choose the class where you want to extract the method'
		].
		cls ifNil: [
			self inform: 'Command aborted, you must select a class.'.
			CmCommandAborted signal ].
		cls ]
]

{ #category : 'execution' }
SycExtractMethodCommand >> setUpOptionToChangeMethodNameDuring: refactoring [

	refactoring setOption: #methodName toUse: [ :methodName :ref |
		| dialog invalidArgs |
		invalidArgs := self computeInvalidArgNamesForSelector: self method selector.
		dialog := nil.
		[ dialog isNil or: [ dialog presenter methodName isValid not ] ] whileTrue: [
			dialog := (StMethodNameEditorPresenter newApplication: StPharoApplication current model: methodName)
			      invalidArgNames: invalidArgs;
			      canRenameArgs: true;
			      canRemoveArgs: false;
			      canAddArgs: false;
			      openModal.
			dialog cancelled ifTrue: [ CmCommandAborted signal ] ].
		methodName ]
]

{ #category : 'execution' }
SycExtractMethodCommand >> setUpOptionToOverrideExistingMethodDuring: aRefactoring [

	aRefactoring setOption: #alreadyDefined toUse:  [ :ref :class :selector |
		ref refactoringWarning: 'Method ', selector printString, ' will override method in ', class name]
]

{ #category : 'execution' }
SycExtractMethodCommand >> setUpOptionsOf: refactoring [

	self setUpOptionToChangeExtractionClass: refactoring.
	self setUpOptionToOverrideExistingMethodDuring: refactoring.
	self setUpOptionToChangeMethodNameDuring: refactoring
]

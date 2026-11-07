"
I am a command to extract selected ast node into separate method 
"
Class {
	#name : 'SycExtractMethodAndOccurrencesCommand',
	#superclass : 'SycSourceCodeRefactoringCommand',
	#instVars : [
		'selectedTextInterval'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycExtractMethodAndOccurrencesCommand class >> canBeExecutedInContext: aSourceCodeContext [

	^ (super canBeExecutedInContext: aSourceCodeContext) and: [
		  aSourceCodeContext isMethodSelected not ]
]

{ #category : 'testing' }
SycExtractMethodAndOccurrencesCommand class >> methodEditorShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $e meta, $m meta for: ClySourceCodeContext
]

{ #category : 'converting' }
SycExtractMethodAndOccurrencesCommand >> asRefactorings [

	| selectedInterval refactoring |
	selectedInterval := (self selectedTextInterval ifNil: [ (1 to: 0) ]) ifEmpty: [
		                    self sourceNode sourceInterval ].
	refactoring := RBExtractMethodAndOccurrences
		               extract: selectedInterval
		               from: self method selector
		               in: self method origin.
	self setUpOptionsOf: refactoring.
	^ { refactoring }
]

{ #category : 'accessing' }
SycExtractMethodAndOccurrencesCommand >> description [

	^ 'Extracts selected ast node into separate method.'
]

{ #category : 'accessing' }
SycExtractMethodAndOccurrencesCommand >> iconName [
	^ #restart
]

{ #category : 'accessing' }
SycExtractMethodAndOccurrencesCommand >> name [
	^ '(Old refactor) Extract method with occurrences'
]

{ #category : 'accessing' }
SycExtractMethodAndOccurrencesCommand >> order [

	^ 1.4
]

{ #category : 'execution' }
SycExtractMethodAndOccurrencesCommand >> prepareFullExecution [

	super prepareFullExecution.
	self setUpModelFromContext: self context.
	selectedTextInterval := self context selectedTextInterval
]

{ #category : 'accessing' }
SycExtractMethodAndOccurrencesCommand >> selectedTextInterval [
	^ selectedTextInterval ifNil: [ self context ifNotNil: [ :ctx | ctx selectedTextInterval ] ]
]

{ #category : 'execution' }
SycExtractMethodAndOccurrencesCommand >> setUpOptionToChangeExtractionClass: refactoring [

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
SycExtractMethodAndOccurrencesCommand >> setUpOptionToChangeMethodNameDuring: refactoring [
	| dialog |
	
	refactoring setOption: #methodName toUse: [ :methodName :ref | | invalidArgs |
		invalidArgs := self computeInvalidArgNamesForSelector: self method selector.
		dialog := (StMethodNameEditorPresenter newApplication: StPharoApplication current model: methodName)
			invalidArgNames: invalidArgs;
			canRenameArgs: true;
			canRemoveArgs: false;
			canAddArgs: false;
			openModal.
		dialog cancelled ifTrue: [ CmCommandAborted signal ].
		methodName ]
]

{ #category : 'execution' }
SycExtractMethodAndOccurrencesCommand >> setUpOptionToOverrideExistingMethodDuring: aRefactoring [

	aRefactoring setOption: #alreadyDefined toUse:  [ :ref :class :selector |
		ref refactoringWarning: 'Method ', selector printString, ' will override method in ', class name]
]

{ #category : 'execution' }
SycExtractMethodAndOccurrencesCommand >> setUpOptionsOf: refactoring [

	self setUpOptionToChangeExtractionClass: refactoring.
	self setUpOptionToOverrideExistingMethodDuring: refactoring.
	self setUpOptionToChangeMethodNameDuring: refactoring.
]

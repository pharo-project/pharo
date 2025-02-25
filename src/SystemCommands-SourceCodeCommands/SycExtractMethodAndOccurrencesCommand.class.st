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

	^CmdShortcutActivation by: $e meta, $m meta for: ClySourceCodeContext
]

{ #category : 'converting' }
SycExtractMethodAndOccurrencesCommand >> asRefactorings [

	| selectedInterval refactoring |
	selectedInterval := selectedTextInterval ifEmpty: [
		                    sourceNode sourceInterval ].
	refactoring := RBExtractMethodAndOccurrences
		               extract: selectedInterval
		               from: method selector
		               in: method origin.
	self setUpOptionsOf: refactoring.
	^ { refactoring }
]

{ #category : 'accessing' }
SycExtractMethodAndOccurrencesCommand >> defaultMenuIconName [
	^ #restart
]

{ #category : 'accessing' }
SycExtractMethodAndOccurrencesCommand >> defaultMenuItemName [
	^ '(Old refactor) Extract method with occurrences'
]

{ #category : 'accessing' }
SycExtractMethodAndOccurrencesCommand >> description [

	^ 'Extracts selected ast node into separate method.'
]

{ #category : 'execution' }
SycExtractMethodAndOccurrencesCommand >> readParametersFromContext: aSourceCodeContext [
	super readParametersFromContext: aSourceCodeContext.
	self setUpModelFromContext: aSourceCodeContext.
	selectedTextInterval := aSourceCodeContext selectedTextInterval
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
			CmdCommandAborted signal ].
		cls ]
]

{ #category : 'execution' }
SycExtractMethodAndOccurrencesCommand >> setUpOptionToChangeMethodNameDuring: refactoring [

	| dialog |
	refactoring setOption: #methodName toUse: [ :methodName :ref | | invalidArgs |
		invalidArgs := self computeInvalidArgNamesForSelector: method selector.
		dialog := StMethodNameEditorPresenter
			openOn: methodName
			withInvalidArgs: invalidArgs
			canRenameArgs: true
			canRemoveArgs: false
			canAddArgs: false.
		dialog cancelled ifTrue: [ CmdCommandAborted signal ].
		methodName ]
]

{ #category : 'execution' }
SycExtractMethodAndOccurrencesCommand >> setUpOptionToOverrideExistingMethodDuring: aRefactoring [

	aRefactoring setOption: #alreadyDefined toUse:  [ :ref :class :selector |
		ref refactoringWarning: 'Method ', selector printString, ' will override method in ', class name]
]

{ #category : 'execution' }
SycExtractMethodAndOccurrencesCommand >> setUpOptionsOf: refactoring [

	self initializeDefaultOptionsOf: refactoring.
	self setUpOptionToChangeExtractionClass: refactoring.
	self setUpOptionToOverrideExistingMethodDuring: refactoring.
	self setUpOptionToChangeMethodNameDuring: refactoring
]

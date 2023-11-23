Class {
	#name : 'SycExtractMethod2Command',
	#superclass : 'SycSourceCodeRefactoringCommand',
	#traits : 'TRefactoringCommandSupport',
	#classTraits : 'TRefactoringCommandSupport classTrait',
	#instVars : [
		'selectedTextInterval'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycExtractMethod2Command class >> canBeExecutedInContext: aSourceCodeContext [

	^ (super canBeExecutedInContext: aSourceCodeContext) and: [
		  aSourceCodeContext isMethodSelected not ]
]

{ #category : 'testing' }
SycExtractMethod2Command class >> methodEditorShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $e meta, $m meta for: ClySourceCodeContext
]

{ #category : 'converting' }
SycExtractMethod2Command >> asRefactorings [

	| selectedInterval refactoring |
	selectedInterval := selectedTextInterval ifEmpty: [
		                    sourceNode sourceInterval ].

	refactoring := RBExtractMethodTransformation
		               extract: (method sourceCode
				                copyFrom: selectedInterval first
				                to: selectedInterval last)
		               from: method selector
		               to: self getNewSelector selector
		               in: method origin.
	self setUpOptionsOf: refactoring.
	^ { refactoring }
]

{ #category : 'accessing' }
SycExtractMethod2Command >> defaultMenuIconName [
	^ #glamorousRestart
]

{ #category : 'accessing' }
SycExtractMethod2Command >> defaultMenuItemName [

	^ 'Extract method 2'
]

{ #category : 'execution' }
SycExtractMethod2Command >> execute [

	self executeRefactoring: self asRefactorings
]

{ #category : 'private' }
SycExtractMethod2Command >> executeRefactoring: refactoring [

	self initializeDefaultOptionsOf: refactoring.
	[ refactoring execute ]
		on: RBRefactoringError
		do: [ :e | UIManager default alert: e messageText ]
]

{ #category : 'converting' }
SycExtractMethod2Command >> getNewSelector [

	| invalidArgs methodName dialog |
	methodName := RBMethodName 
							selector: #fooForNow
		  					arguments: #().
	invalidArgs := self computeInvalidArgNamesForSelector: method selector.
	dialog := SycMethodNameEditorPresenter
		          openOn: methodName
		          withInvalidArgs: invalidArgs
		          canRenameArgs: true
		          canRemoveArgs: false
		          canAddArgs: false.
	dialog cancelled ifTrue: [ CmdCommandAborted signal ].
	^ methodName
]

{ #category : 'execution' }
SycExtractMethod2Command >> readParametersFromContext: aSourceCodeContext [
	super readParametersFromContext: aSourceCodeContext.
	self setUpModelFromContext: aSourceCodeContext.
	selectedTextInterval := aSourceCodeContext selectedTextInterval
]

{ #category : 'execution' }
SycExtractMethod2Command >> setUpOptionToChangeExtractionClass: refactoring [

	refactoring setOption: #extractionClass toUse: [ :ref | | cls superclasses |
		cls := ref methodClass realClass.
		superclasses := cls allSuperclasses reversed copyWithoutAll: { Object . ProtoObject }.
		superclasses ifNotEmpty: [
			cls := UIManager default
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
SycExtractMethod2Command >> setUpOptionToChangeMethodNameDuring: refactoring [

	| dialog |
	refactoring setOption: #methodName toUse: [ :methodName :ref | | invalidArgs |
		invalidArgs := self computeInvalidArgNamesForSelector: method selector.
		dialog := SycMethodNameEditorPresenter
			openOn: methodName
			withInvalidArgs: invalidArgs
			canRenameArgs: true
			canRemoveArgs: false
			canAddArgs: false.
		dialog cancelled ifTrue: [ CmdCommandAborted signal ].
		methodName ]
]

{ #category : 'execution' }
SycExtractMethod2Command >> setUpOptionToOverrideExistingMethodDuring: aRefactoring [

	aRefactoring setOption: #alreadyDefined toUse:  [ :ref :class :selector |
		ref refactoringWarning: 'Method ', selector printString, ' will override method in ', class name]
]

{ #category : 'execution' }
SycExtractMethod2Command >> setUpOptionsOf: refactoring [

	self initializeDefaultOptionsOf: refactoring.
	self setUpOptionToChangeExtractionClass: refactoring.

	self setUpOptionToOverrideExistingMethodDuring: refactoring.
	self setUpOptionToChangeMethodNameDuring: refactoring
]

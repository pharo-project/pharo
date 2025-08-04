"
I am a command to extract selected ast node into separate method 
"
Class {
	#name : 'SycNewExtractMethodCommand',
	#superclass : 'SycSourceCodeRefactoringCommand',
	#instVars : [
		'selectedTextInterval',
		'refactoringScopes'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycNewExtractMethodCommand class >> canBeExecutedInContext: aSourceCodeContext [

	^ true. "(super canBeExecutedInContext: aSourceCodeContext) and: [
		  aSourceCodeContext isMethodSelected not ]"
]

{ #category : 'testing' }
SycNewExtractMethodCommand class >> methodEditorShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $e meta, $m meta, $t meta for: ClySourceCodeContext
]

{ #category : 'converting' }
SycNewExtractMethodCommand >> asRefactorings [

	^{self createRefactoring}
]

{ #category : 'converting' }
SycNewExtractMethodCommand >> createRefactoring [
	"not used"
	^ self
]

{ #category : 'accessing' }
SycNewExtractMethodCommand >> defaultMenuIconName [
	^ #restart
]

{ #category : 'accessing' }
SycNewExtractMethodCommand >> defaultMenuItemName [

	^ '(NewImplementation) Extract method'
]

{ #category : 'accessing' }
SycNewExtractMethodCommand >> description [

	^ 'Extract selected ast node into separate method.'
]

{ #category : 'converting' }
SycNewExtractMethodCommand >> execute [

	| selectedInterval driver |
	selectedInterval := selectedTextInterval ifEmpty: [
		                    sourceNode sourceInterval ].
	driver := ReExtractMethodDriver new 
							scopes: refactoringScopes;
							model: model;
		               extract: (method sourceCode
				                		copyFrom: selectedInterval first
				               			to: selectedInterval last)
		               from: method
		               in: method origin.
	driver runRefactoring.
	
]

{ #category : 'converting' }
SycNewExtractMethodCommand >> getNewSelector [

	| invalidArgs methodName dialog |
	methodName := RBMethodName 
							selector: #fooForNow
		  					arguments: #().
	invalidArgs := self computeInvalidArgNamesForSelector: method selector.
	dialog := StMethodNameEditorPresenter
		          openOn: methodName
		          withInvalidArgs: invalidArgs
		          canRenameArgs: true
		          canRemoveArgs: false
		          canAddArgs: false.
	dialog cancelled ifTrue: [ CmdCommandAborted signal ].
	^ methodName
]

{ #category : 'testing' }
SycNewExtractMethodCommand >> isComplexRefactoring [
	^ false
]

{ #category : 'execution' }
SycNewExtractMethodCommand >> readParametersFromContext: aSourceCodeContext [
	super readParametersFromContext: aSourceCodeContext.
	self setUpModelFromContext: aSourceCodeContext.
	selectedTextInterval := aSourceCodeContext selectedTextInterval
]

{ #category : 'initialization' }
SycNewExtractMethodCommand >> setUpModelFromContext: aContext [
	| browser scope |
	
	browser := aContext browser.
	scope := browser defaultNavigationScope.
	scope isCurrentImage ifTrue: [ scope := ScopesManager defaultScope ].
	model := RBNamespace onEnvironment: scope asRBEnvironment.
	refactoringScopes := aContext refactoringScopes.
]

{ #category : 'execution' }
SycNewExtractMethodCommand >> setUpOptionToChangeExtractionClass: refactoring [

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
SycNewExtractMethodCommand >> setUpOptionToChangeMethodNameDuring: refactoring [

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
SycNewExtractMethodCommand >> setUpOptionToOverrideExistingMethodDuring: aRefactoring [

	aRefactoring setOption: #alreadyDefined toUse:  [ :ref :class :selector |
		ref refactoringWarning: 'Method ', selector printString, ' will override method in ', class name]
]

{ #category : 'execution' }
SycNewExtractMethodCommand >> setUpOptionsOf: refactoring [

	self initializeDefaultOptionsOf: refactoring.
	self setUpOptionToChangeExtractionClass: refactoring.

	self setUpOptionToOverrideExistingMethodDuring: refactoring.
	self setUpOptionToChangeMethodNameDuring: refactoring
]

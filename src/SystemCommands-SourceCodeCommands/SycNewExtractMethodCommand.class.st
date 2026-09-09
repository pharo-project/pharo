"
I am a command to extract selected ast node into separate method 
"
Class {
	#name : 'SycNewExtractMethodCommand',
	#superclass : 'SycSourceCodeRefactoringCommand',
	#instVars : [
		'selectedTextInterval'
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

{ #category : 'accessing' }
SycNewExtractMethodCommand >> defaultMenuIconName [
	^ #restart
]

{ #category : 'accessing' }
SycNewExtractMethodCommand >> defaultMenuItemName [

	^ '(R) Extract method (no occurrences)'
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

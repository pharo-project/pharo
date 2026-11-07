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

	^ClyShortcutActivation by: $e meta, $m meta, $t meta for: ClySourceCodeContext
]

{ #category : 'accessing' }
SycNewExtractMethodCommand >> description [

	^ 'Extract selected ast node into separate method.'
]

{ #category : 'execution' }
SycNewExtractMethodCommand >> executeRefactoring [

	| selectedInterval driver |
	selectedInterval := self selectedTextInterval ifEmpty: [
					self sourceNode sourceInterval ].
	driver := ReExtractMethodDriver new
							scopes: refactoringScopes;
							model: model;
							extract: (self method sourceCode
										copyFrom: selectedInterval first
										to: selectedInterval last)
							   from: self method
							   in: self method origin.
	driver runRefactoring
]

{ #category : 'accessing' }
SycNewExtractMethodCommand >> iconName [
	^ #restart
]

{ #category : 'accessing' }
SycNewExtractMethodCommand >> name [

	^ '(R) Extract method (no occurrences)'
]

{ #category : 'accessing' }
SycNewExtractMethodCommand >> order [

	^ 1.4
]

{ #category : 'execution' }
SycNewExtractMethodCommand >> prepareFullExecution [

	super prepareFullExecution.
	self setUpModelFromContext: self context.
	selectedTextInterval := self context selectedTextInterval
]

{ #category : 'accessing' }
SycNewExtractMethodCommand >> selectedTextInterval [
	^ selectedTextInterval ifNil: [ self context ifNotNil: [ :ctx | ctx selectedTextInterval ] ]
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

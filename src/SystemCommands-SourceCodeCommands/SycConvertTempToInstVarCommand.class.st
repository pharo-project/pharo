"
I am a command to convert a temp variable in instance variable.
"
Class {
	#name : 'SycConvertTempToInstVarCommand',
	#superclass : 'SycSourceCodeRefactoringCommand',
	#instVars : [
		'refactoringScopes'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycConvertTempToInstVarCommand class >> canBeExecutedInContext: aSourceCodeContext [
	^(super canBeExecutedInContext: aSourceCodeContext) and: [aSourceCodeContext isTempVariableSelected]
]

{ #category : 'execution' }
SycConvertTempToInstVarCommand >> defaultMenuIconName [
	^ #smallRedo
]

{ #category : 'execution' }
SycConvertTempToInstVarCommand >> defaultMenuItemName [
	^'(R) Convert to Instance Var'
]

{ #category : 'execution' }
SycConvertTempToInstVarCommand >> execute [
	
	self executeRefactorings 
]

{ #category : 'execution' }
SycConvertTempToInstVarCommand >> executeRefactorings [
	
	^ (ReConvertTemporaryToInstanceVariableDriver new 
		scopes: refactoringScopes 
		class: method origin
		selector: method selector
		variable: sourceNode name) runRefactoring
		
]

{ #category : 'execution' }
SycConvertTempToInstVarCommand >> isComplexRefactoring [

	^ false
]

{ #category : 'execution' }
SycConvertTempToInstVarCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	refactoringScopes := aToolContext refactoringScopes.
]

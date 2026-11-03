"
I am a command to convert a temp variable in instance variable.
"
Class {
	#name : 'SycConvertTempToInstVarCommand',
	#superclass : 'SycSourceCodeRefactoringCommand',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycConvertTempToInstVarCommand class >> canBeExecutedInContext: aSourceCodeContext [
	^(super canBeExecutedInContext: aSourceCodeContext) and: [aSourceCodeContext isTempVariableSelected]
]

{ #category : 'execution' }
SycConvertTempToInstVarCommand >> execute [
	
	self executeRefactorings 
]

{ #category : 'execution' }
SycConvertTempToInstVarCommand >> executeRefactorings [

	^ ((ReConvertTemporaryToInstanceVariableDriver newApplication: self application)
		   scopes: refactoringScopes
		   class: method origin
		   selector: method selector
		   variable: sourceNode name) runRefactoring
]

{ #category : 'execution' }
SycConvertTempToInstVarCommand >> iconName [
	^ #smallRedo
]

{ #category : 'execution' }
SycConvertTempToInstVarCommand >> name [
	^'(R) Convert to Instance Var'
]

"
I am a command to generate accessors for all inst vars of given classes
"
Class {
	#name : 'SycGenerateAllInstVarAccessorsCommand',
	#superclass : 'SycClassCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'execution' }
SycGenerateAllInstVarAccessorsCommand >> asRefactorings [

	| refactorings |
	refactorings := classes flatCollect: [ :eachClass |
		                eachClass slots collect: [ :var |
			                | accesorRefactoring |
			                accesorRefactoring := RBAddVariableAccessorTransformation
				                                      instanceVariable: var name
				                                      class: eachClass.
			                accesorRefactoring ] ].
	^ refactorings
]

{ #category : 'execution' }
SycGenerateAllInstVarAccessorsCommand >> execute [

	self executeRefactorings
]

{ #category : 'accessing' }
SycGenerateAllInstVarAccessorsCommand >> iconName [
	^ #smallAdd
]

{ #category : 'testing' }
SycGenerateAllInstVarAccessorsCommand >> isComplexRefactoring [
	^true
]

{ #category : 'accessing' }
SycGenerateAllInstVarAccessorsCommand >> name [
	^'Generate accessors'
]

{ #category : 'execution' }
SycGenerateAllInstVarAccessorsCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	classes := classes collect: [:each | aToolContext currentMetaLevelOf: each]
]

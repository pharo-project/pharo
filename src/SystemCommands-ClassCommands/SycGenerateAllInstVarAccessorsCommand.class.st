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

{ #category : 'accessing' }
SycGenerateAllInstVarAccessorsCommand >> fullBrowserOrder [

	^ 2.1
]

{ #category : 'accessing' }
SycGenerateAllInstVarAccessorsCommand >> iconName [
	^ #smallAdd
]

{ #category : 'testing' }
SycGenerateAllInstVarAccessorsCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserClassContext
]

{ #category : 'testing' }
SycGenerateAllInstVarAccessorsCommand >> isComplexRefactoring [
	^true
]

{ #category : 'accessing' }
SycGenerateAllInstVarAccessorsCommand >> name [
	^'Generate accessors'
]

{ #category : 'accessing' }
SycGenerateAllInstVarAccessorsCommand >> order [

	^ 2.1
]

{ #category : 'preparation' }
SycGenerateAllInstVarAccessorsCommand >> prepareFullExecution [

	super prepareFullExecution.
	classes := classes collect: [ :class | self context currentMetaLevelOf: class ]
]

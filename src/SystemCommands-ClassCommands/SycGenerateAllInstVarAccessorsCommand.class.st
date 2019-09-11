"
I am a command to generate accessors for all inst vars of given classes
"
Class {
	#name : #SycGenerateAllInstVarAccessorsCommand,
	#superclass : #SycClassCommand,
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #activation }
SycGenerateAllInstVarAccessorsCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation  by: $g meta, $a meta for: ClyClass asCalypsoItemContext 
]

{ #category : #execution }
SycGenerateAllInstVarAccessorsCommand >> asRefactorings [

	| refactorings |
	refactorings := classes flatCollect: [:eachClass |
		eachClass	 slots collect: [:var | 
			var createRefactoring: RBCreateAccessorsForVariableRefactoring for: eachClass]].
	refactorings do: [ :each | 
		each classVariable: false].
	^refactorings
]

{ #category : #accessing }
SycGenerateAllInstVarAccessorsCommand >> defaultMenuIconName [ 
	^ #smallAdd
]

{ #category : #accessing }
SycGenerateAllInstVarAccessorsCommand >> defaultMenuItemName [
	^'Generate accessors'
]

{ #category : #execution }
SycGenerateAllInstVarAccessorsCommand >> execute [

	self executeRefactorings
]

{ #category : #testing }
SycGenerateAllInstVarAccessorsCommand >> isComplexRefactoring [
	^true
]

{ #category : #execution }
SycGenerateAllInstVarAccessorsCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	classes := classes collect: [:each | aToolContext currentMetaLevelOf: each]
]

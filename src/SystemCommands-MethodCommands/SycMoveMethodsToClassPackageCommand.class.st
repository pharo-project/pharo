"
I am a command to move method to the package of defining class.

I am used to convert extension method to normal one
"
Class {
	#name : #SycMoveMethodsToClassPackageCommand,
	#superclass : #SycMethodRepackagingCommand,
	#instVars : [
		'targetTagName'
	],
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #testing }
SycMoveMethodsToClassPackageCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isExtensionMethodSelected
]

{ #category : #activation }
SycMoveMethodsToClassPackageCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: ClyMoreRefactorsMenuGroup 
		order: 80 
		for: ClyMethod asCalypsoItemContext
]

{ #category : #accessing }
SycMoveMethodsToClassPackageCommand >> defaultMenuIconName [ 
	^ #smallRedo
]

{ #category : #accessing }
SycMoveMethodsToClassPackageCommand >> defaultMenuItemName [
	^'Move to class package'
]

{ #category : #execution }
SycMoveMethodsToClassPackageCommand >> execute [

	| classPackage |
	methods do: [ :each | 
		classPackage := each origin package.
		self moveMethod: each toPackage: classPackage.
		each tagWith: targetTagName]
]

{ #category : #execution }
SycMoveMethodsToClassPackageCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	targetTagName := aToolContext requestSingleMethodTag: 'Choose protocol for methods'
]

{ #category : #accessing }
SycMoveMethodsToClassPackageCommand >> targetTagName [
	^ targetTagName
]

{ #category : #accessing }
SycMoveMethodsToClassPackageCommand >> targetTagName: anObject [
	targetTagName := anObject
]

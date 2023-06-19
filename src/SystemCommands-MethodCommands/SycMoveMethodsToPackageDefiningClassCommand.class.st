"
I am a command to move method to the package of defining class.

I am used to convert extension method to normal one
"
Class {
	#name : #SycMoveMethodsToPackageDefiningClassCommand,
	#superclass : #SycMethodRepackagingCommand,
	#instVars : [
		'targetTagName'
	],
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #testing }
SycMoveMethodsToPackageDefiningClassCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isExtensionMethodSelected
]

{ #category : #accessing }
SycMoveMethodsToPackageDefiningClassCommand >> defaultMenuIconName [
	^ #smallRedo
]

{ #category : #accessing }
SycMoveMethodsToPackageDefiningClassCommand >> defaultMenuItemName [
	^'Move to package defining the class'
]

{ #category : #execution }
SycMoveMethodsToPackageDefiningClassCommand >> execute [

	| classPackage |
	methods do: [ :method |
		classPackage := method origin package.
		self moveMethod: method toPackage: classPackage.
		method protocol: targetTagName ]
]

{ #category : #execution }
SycMoveMethodsToPackageDefiningClassCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	targetTagName := aToolContext requestSingleMethodTag: 'Choose protocol for methods'
]

{ #category : #accessing }
SycMoveMethodsToPackageDefiningClassCommand >> targetTagName [
	^ targetTagName
]

{ #category : #accessing }
SycMoveMethodsToPackageDefiningClassCommand >> targetTagName: anObject [
	targetTagName := anObject
]

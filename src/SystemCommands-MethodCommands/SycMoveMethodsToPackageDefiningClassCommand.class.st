"
I am a command to move method to the package of defining class.

I am used to convert extension method to normal one
"
Class {
	#name : #SycMoveMethodsToPackageDefiningClassCommand,
	#superclass : #SycMethodRepackagingCommand,
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

	methods
		select: [ :method | method isExtensionMethod ]
		thenDo: [ :method | self tagMethod: method ]
]

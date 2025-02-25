"
I am a command to move method to another package.

I am used to convert method to extension.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	package:		<Package>
"
Class {
	#name : 'SycMoveMethodsToPackageCommand',
	#superclass : 'SycMethodRepackagingCommand',
	#instVars : [
		'package'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'instance creation' }
SycMoveMethodsToPackageCommand class >> for: methods to: aPackage [
	^(self for: methods)
		package: aPackage
]

{ #category : 'accessing' }
SycMoveMethodsToPackageCommand >> defaultMenuIconName [
	^ #smallRedo
]

{ #category : 'accessing' }
SycMoveMethodsToPackageCommand >> defaultMenuItemName [
	^'Move to package'
]

{ #category : 'execution' }
SycMoveMethodsToPackageCommand >> execute [

	methods do: [ :each |
		self moveMethod: each toPackage: package.
		each origin removeEmptyProtocols ]
]

{ #category : 'execution' }
SycMoveMethodsToPackageCommand >> moveMethod: aMethod toPackage: aPackage [

	aMethod package = aPackage ifTrue: [ ^ self ].

	"If true it means it is an extension"
	aPackage ~~ aMethod origin package
		ifTrue: [ aMethod protocol: '*' , aPackage name ]
		ifFalse: [ self classifyMethod: aMethod ]
]

{ #category : 'accessing' }
SycMoveMethodsToPackageCommand >> package [
	^ package
]

{ #category : 'accessing' }
SycMoveMethodsToPackageCommand >> package: anObject [
	package := anObject
]

{ #category : 'execution' }
SycMoveMethodsToPackageCommand >> prepareExecutionInDropContext: aToolContext [
	super prepareExecutionInDropContext: aToolContext.
	package := aToolContext lastSelectedPackage
]

{ #category : 'execution' }
SycMoveMethodsToPackageCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	package := aToolContext requestSinglePackage: 'Choose package'
]

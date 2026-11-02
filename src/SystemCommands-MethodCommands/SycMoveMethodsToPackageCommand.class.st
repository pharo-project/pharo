"
I am a command to move method to another package.

I am used to convert method to extension.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	package:		<Package>
"
Class {
	#name : 'SycMoveMethodsToPackageCommand',
	#superclass : 'SycMethodRepackagingCmCommand',
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

{ #category : 'execution' }
SycMoveMethodsToPackageCommand >> executeRefactoring [

	self methods do: [ :method |
			self moveMethod: method toPackage: package.
			method origin removeEmptyProtocols ]
]

{ #category : 'accessing' }
SycMoveMethodsToPackageCommand >> icon [

	^ self iconNamed: #smallRedo
]

{ #category : 'accessing' }
SycMoveMethodsToPackageCommand >> iconName [
	^ #smallRedo
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
SycMoveMethodsToPackageCommand >> name [

	^ 'Move to package'
]

{ #category : 'accessing' }
SycMoveMethodsToPackageCommand >> order [

	^ 70
]

{ #category : 'accessing' }
SycMoveMethodsToPackageCommand >> package [
	^ package
]

{ #category : 'accessing' }
SycMoveMethodsToPackageCommand >> package: anObject [
	package := anObject
]

{ #category : 'executing' }
SycMoveMethodsToPackageCommand >> prepareFullExecution [
	super prepareFullExecution.
	package := self dropContext
		ifNil: [ self context requestSinglePackage: 'Choose package' ]
		ifNotNil: [ :drop | drop lastSelectedPackage ]
]

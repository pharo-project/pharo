"
I am a command to add new package into the system.

I should be created on particular system environment which allow create packages:

 	resultPackage := systemEnvironment createPackageNamed: packageName
	
Currently only Calypso provide such compatible environment (ClySystemEnvironment)	
 
Internal Representation and Key Implementation Points.

    Instance Variables
	packageName:		<String>
	resultPackage:		<Package>
	systemEnvironment:		<Object>
"
Class {
	#name : 'SycAddNewPackageCommand',
	#superclass : 'SycPackageCommand',
	#instVars : [
		'systemEnvironment',
		'packageName',
		'resultPackage'
	],
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'testing' }
SycAddNewPackageCommand class >> canBeExecutedInContext: aToolContext [

	^ true
]

{ #category : 'execution' }
SycAddNewPackageCommand >> applyCommandResult [
	super applyCommandResult.
	self context showPackage: resultPackage
]

{ #category : 'accessing' }
SycAddNewPackageCommand >> description [

	^ 'Adds a new package into the system.'
]

{ #category : 'execution' }
SycAddNewPackageCommand >> executeRefactoring [

	resultPackage := [ systemEnvironment createPackageNamed: packageName ]
		                 on: PackageConflictError
		                 do: [ systemEnvironment packageNamed: packageName ]
]

{ #category : 'accessing' }
SycAddNewPackageCommand >> iconName [
	^#add
]

{ #category : 'accessing' }
SycAddNewPackageCommand >> name [
	^'New package'
]

{ #category : 'accessing' }
SycAddNewPackageCommand >> order [

	^ 1000
]

{ #category : 'accessing' }
SycAddNewPackageCommand >> packageName [
	^ packageName
]

{ #category : 'accessing' }
SycAddNewPackageCommand >> packageName: anObject [
	packageName := anObject
]

{ #category : 'execution' }
SycAddNewPackageCommand >> prepareFullExecution [

	systemEnvironment := self context systemEnvironment.
	packageName := ''.
	self context isPackageSelected ifTrue: [ packageName := self context lastSelectedPackage name ].
	packageName := self morphicUIManager request: 'Name of the new package:' initialAnswer: packageName title: 'Create a new package'.
	packageName isEmptyOrNil ifTrue: [ ^ CmCommandAborted signal ]
]

{ #category : 'accessing' }
SycAddNewPackageCommand >> resultPackage [
	^ resultPackage
]

{ #category : 'accessing' }
SycAddNewPackageCommand >> resultPackage: anObject [
	resultPackage := anObject
]

{ #category : 'accessing' }
SycAddNewPackageCommand >> systemEnvironment [
	^ systemEnvironment
]

{ #category : 'accessing' }
SycAddNewPackageCommand >> systemEnvironment: anObject [
	systemEnvironment := anObject
]

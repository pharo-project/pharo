"
I am a command to add new package into the system.

I should be created on particular system environment which allow create packages:

 	resultPackage := systemEnvironment createPackageNamed: packageName
	
Currently only Calypso provide such compatible environment (ClySystemEnvironment)	
 
Internal Representation and Key Implementation Points.

    Instance Variables
	packageName:		<String>
	resultPackage:		<RPackage>
	systemEnvironment:		<Object>
"
Class {
	#name : #SycAddNewPackageCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'systemEnvironment',
		'packageName',
		'resultPackage'
	],
	#category : #'SystemCommands-PackageCommands'
}

{ #category : #execution }
SycAddNewPackageCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	aToolContext showPackage: resultPackage
]

{ #category : #accessing }
SycAddNewPackageCommand >> defaultMenuIconName [
	^#add
]

{ #category : #accessing }
SycAddNewPackageCommand >> defaultMenuItemName [
	^'New package'
]

{ #category : #execution }
SycAddNewPackageCommand >> execute [

	resultPackage := systemEnvironment createPackageNamed: packageName
]

{ #category : #accessing }
SycAddNewPackageCommand >> packageName [
	^ packageName
]

{ #category : #accessing }
SycAddNewPackageCommand >> packageName: anObject [
	packageName := anObject
]

{ #category : #execution }
SycAddNewPackageCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
		
	systemEnvironment := aToolContext systemEnvironment.		
	packageName := ''.
	aToolContext isPackageSelected ifTrue: [ 
		packageName := aToolContext lastSelectedPackage name].
	
	packageName := UIManager default 
		request: 'Name of the new package:' initialAnswer: packageName title: 'Create a new package'.
	packageName isEmptyOrNil ifTrue: [ ^CmdCommandAborted signal]
]

{ #category : #accessing }
SycAddNewPackageCommand >> resultPackage [
	^ resultPackage
]

{ #category : #accessing }
SycAddNewPackageCommand >> resultPackage: anObject [
	resultPackage := anObject
]

{ #category : #accessing }
SycAddNewPackageCommand >> systemEnvironment [
	^ systemEnvironment
]

{ #category : #accessing }
SycAddNewPackageCommand >> systemEnvironment: anObject [
	systemEnvironment := anObject
]

"
I am a base class for commands which perform operations with given packages.

Internal Representation and Key Implementation Points.

    Instance Variables
	packages:		<Collection of<Package>>
"
Class {
	#name : 'SycPackageCommand',
	#superclass : 'ClyCommand',
	#instVars : [
		'packages'
	],
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'testing' }
SycPackageCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isPackageSelected
]

{ #category : 'testing' }
SycPackageCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserPackageContext
]

{ #category : 'accessing' }
SycPackageCommand >> packages [
	^ packages
]

{ #category : 'accessing' }
SycPackageCommand >> packages: anObject [
	packages := anObject
]

{ #category : 'execution' }
SycPackageCommand >> prepareFullExecution [

	packages := self context selectedPackages
]

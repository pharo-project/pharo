"
I am a base class for commands which perform operations with given packages.

Internal Representation and Key Implementation Points.

    Instance Variables
	packages:		<Collection of<RPackage>>
"
Class {
	#name : #SycPackageCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'packages'
	],
	#category : #'SystemCommands-PackageCommands'
}

{ #category : #testing }
SycPackageCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isPackageSelected
]

{ #category : #testing }
SycPackageCommand class >> isAbstract [
	^self = SycPackageCommand 
]

{ #category : #accessing }
SycPackageCommand >> packages [
	^ packages
]

{ #category : #accessing }
SycPackageCommand >> packages: anObject [
	packages := anObject
]

{ #category : #execution }
SycPackageCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	packages := aToolContext selectedPackages
]

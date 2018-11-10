"
Abstract class representing commands to be applied on top of packages.
"
Class {
	#name : #IceTipPackageCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'packageModel'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipPackageCommand class >> isAbstract [

	^ self == IceTipPackageCommand
]

{ #category : #accessing }
IceTipPackageCommand >> packageModel [
	^ packageModel
]

{ #category : #execution }
IceTipPackageCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.	
	packageModel := aToolContext packageModel
]

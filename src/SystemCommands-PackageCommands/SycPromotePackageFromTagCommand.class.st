"
I am a command to promote given package and tag to be separate package.

Internal Representation and Key Implementation Points.

    Instance Variables
	classTag:		<Symbol>
	package:		<Package>

"
Class {
	#name : 'SycPromotePackageFromTagCommand',
	#superclass : 'CmdCommand',
	#instVars : [
		'package',
		'classTag'
	],
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'testing' }
SycPromotePackageFromTagCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isPackageSelected and: [aToolContext isPackageTagSelected]
]

{ #category : 'execution' }
SycPromotePackageFromTagCommand >> applyResultInContext: aBrowserContext [

	| newPackage |
	newPackage := (package name, '-', classTag) asPackage.
	aBrowserContext showPackage: newPackage
]

{ #category : 'accessing' }
SycPromotePackageFromTagCommand >> defaultMenuItemName [
	^'Promote as package'
]

{ #category : 'execution' }
SycPromotePackageFromTagCommand >> execute [

	(package classTagNamed: classTag) promoteAsPackage
]

{ #category : 'execution' }
SycPromotePackageFromTagCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	package := aToolContext lastSelectedPackage.
	classTag := aToolContext lastSelectedPackageTag
]

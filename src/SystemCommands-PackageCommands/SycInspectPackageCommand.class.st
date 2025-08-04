"
I am a command to inspect given package.

Internal Representation and Key Implementation Points.

Instance Variables

package:		<Package>
"
Class {
	#name : 'SycInspectPackageCommand',
	#superclass : 'CmdCommand',
	#instVars : [
		'package'
	],
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'testing' }
SycInspectPackageCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isPackageSelected
]

{ #category : 'activation' }
SycInspectPackageCommand class >> fullBrowserMenuActivation [

	<classAnnotation>
	^ CmdContextMenuActivation
		  byItemOf: CmdExtraMenuGroup
		  for: Package asCalypsoItemContext
]

{ #category : 'accessing' }
SycInspectPackageCommand >> defaultMenuIconName [
	^ #inspect
]

{ #category : 'accessing' }
SycInspectPackageCommand >> defaultMenuItemName [
	^'Inspect'
]

{ #category : 'accessing' }
SycInspectPackageCommand >> description [

	^ 'Inspect package'
]

{ #category : 'execution' }
SycInspectPackageCommand >> execute [

	(Smalltalk tools toolNamed: #inspector) openOn: package
]

{ #category : 'execution' }
SycInspectPackageCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.

	package := aToolContext lastSelectedPackage
]

"
I'm a command to install a baseline (default group)
"
Class {
	#name : #IceTipMetacelloInstallBaselineDefaultCommand,
	#superclass : #IceTipMetacelloInstallBaselineCommand,
	#category : #'Iceberg-Plugin-Metacello'
}

{ #category : #activation }
IceTipMetacelloInstallBaselineDefaultCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
			byItemOf: IceTipMetacelloMenuGroup
			order: 10
			for: IceTipWorkingCopyContext
]

{ #category : #testing }
IceTipMetacelloInstallBaselineDefaultCommand class >> canBeExecutedInContext: aToolContext [

	^ aToolContext packageModel name beginsWith: 'BaselineOf'
]

{ #category : #accessing }
IceTipMetacelloInstallBaselineDefaultCommand class >> defaultMenuIconName [

	^ #smallLoadProject
]

{ #category : #accessing }
IceTipMetacelloInstallBaselineDefaultCommand class >> defaultMenuItemName [
	^ 'Install baseline (default)'
]

{ #category : #execution }
IceTipMetacelloInstallBaselineDefaultCommand >> execute [
	self installBaseline: self package
]

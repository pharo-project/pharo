"
I'm a command to install a baseline (choose the group(s) to install)
"
Class {
	#name : #IceTipMetacelloInstallBaselineGroupCommand,
	#superclass : #IceTipMetacelloInstallBaselineCommand,
	#category : #'Iceberg-Plugin-Metacello'
}

{ #category : #activation }
IceTipMetacelloInstallBaselineGroupCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
			byItemOf: IceTipMetacelloMenuGroup
			order: 20
			for: IceTipWorkingCopyContext
]

{ #category : #testing }
IceTipMetacelloInstallBaselineGroupCommand class >> canBeExecutedInContext: aToolContext [

	^ aToolContext packageModel name beginsWith: 'BaselineOf'
]

{ #category : #accessing }
IceTipMetacelloInstallBaselineGroupCommand class >> defaultMenuIconName [

	^ #smallLoadProject
]

{ #category : #accessing }
IceTipMetacelloInstallBaselineGroupCommand class >> defaultMenuItemName [
	^ 'Install baseline...'
]

{ #category : #execution }
IceTipMetacelloInstallBaselineGroupCommand >> execute [
	| result |
	
	result := UIManager default 
		request: 'Groups to install (a comma separated string)' 
		initialAnswer: '' 
		title: 'Enter groups to install'.
	result ifNil: [ ^ self ].
	
	self 
		installBaseline: self package
		groups: ((result substrings: ',') 
			collect: #trimmed 
			as: Array)
]

"
I am status bar tool for method editors to switch the extension status of the editing method. 

Internal Representation and Key Implementation Points.

    Instance Variables
	checkbox:		<Object>

"
Class {
	#name : 'ClyMethodExtensionSwitchMorph',
	#superclass : 'ClyStatusBarItemMorph',
	#instVars : [
		'checkbox'
	],
	#category : 'Calypso-SystemTools-Core-Editors-Methods',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Methods'
}

{ #category : 'initialization' }
ClyMethodExtensionSwitchMorph >> build [

	checkbox := self theme
		newCheckboxIn: self
		for: self
		getSelected: #isActive
		setSelected: #toggle
		getEnabled: nil
		getLabel: #label
		help: 'toggle check box to choose/reset package of method'.
	checkbox
		changeTableLayout;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap.
	self addMorph: checkbox
]

{ #category : 'accessing' }
ClyMethodExtensionSwitchMorph >> checkbox [
	^ checkbox
]

{ #category : 'accessing' }
ClyMethodExtensionSwitchMorph >> checkbox: anObject [
	checkbox := anObject
]

{ #category : 'testing' }
ClyMethodExtensionSwitchMorph >> isActive [

	^ ownerTool extendingPackage isNotNil
]

{ #category : 'accessing' }
ClyMethodExtensionSwitchMorph >> label [
	| label |
	label := ownerTool extendingPackage
		ifNil: ['extension']
		ifNotNil: [:package | package name ].
	ownerTool modifiesExtension ifTrue: [ label := '*', label ].
	^label
]

{ #category : 'operations' }
ClyMethodExtensionSwitchMorph >> toggle [

	[ownerTool toggleExtendingPackage] on: CmdCommandAborted, CmCommandAborted do: [ :err | ].
	checkbox updateLabel
]

"
I'm a command to ""browse"" a change definition (method, class or package).
"
Class {
	#name : #IceTipBrowseCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'item'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipBrowseCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemFor: IceTipDiffContext
]

{ #category : #activation }
IceTipBrowseCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $b meta for: IceTipDiffContext
]

{ #category : #accessing }
IceTipBrowseCommand class >> defaultHelp [
	^ 'Browse the selected item'
]

{ #category : #accessing }
IceTipBrowseCommand class >> defaultMenuIconName [
	^ #smallSystemBrowser
]

{ #category : #accessing }
IceTipBrowseCommand class >> defaultMenuItemName [
	^ 'Browse...'
]

{ #category : #execution }
IceTipBrowseCommand >> execute [
	item ifNil: [ ^ self ].
	item value definition browse
]

{ #category : #execution }
IceTipBrowseCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	item := aToolContext item
]

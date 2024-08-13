"
Copy the class name to the clipboard
"
Class {
	#name : 'ClyCopyClassNameCommand',
	#superclass : 'ClyBrowserCommand',
	#instVars : [
		'selectedClassItem'
	],
	#category : 'Calypso-Browser-Commands',
	#package : 'Calypso-Browser',
	#tag : 'Commands'
}

{ #category : 'activations' }
ClyCopyClassNameCommand class >> browserContextMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClyBrowserContext
]

{ #category : 'accessing' }
ClyCopyClassNameCommand >> defaultMenuIconName [
	^#smallCopy
]

{ #category : 'accessing' }
ClyCopyClassNameCommand >> defaultMenuItemName [

	^'Copy class name to Clipboard'
]

{ #category : 'execution' }
ClyCopyClassNameCommand >> execute [
	| name |
	name := selectedClassItem actualObject name.
	Clipboard clipboardText: name.
	self inform: ('The class name {1} is now in the clipboard' format: (Array with: name))
]

{ #category : 'execution' }
ClyCopyClassNameCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	selectedClassItem := aToolContext lastSelectedItem
]

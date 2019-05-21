Class {
	#name : #SpecCodePrintItCommand,
	#superclass : #SpecCodeSelectionCommand,
	#category : #'Spec-Commands-Code'
}

{ #category : #activating }
SpecCodePrintItCommand class >> contextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemOrder: 10 for: SpecCodeContext
]

{ #category : #activating }
SpecCodePrintItCommand class >> shortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation 
		by: $p meta
		for: SpecCodeContext
]

{ #category : #accessing }
SpecCodePrintItCommand >> defaultMenuItemName [

	^ 'Print it'
]

{ #category : #execution }
SpecCodePrintItCommand >> execute [

	'OK' crLog
]

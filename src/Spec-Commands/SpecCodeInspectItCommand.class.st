Class {
	#name : #SpecCodeInspectItCommand,
	#superclass : #SpecCodeSelectionCommand,
	#category : #'Spec-Commands-Code'
}

{ #category : #activating }
SpecCodeInspectItCommand class >> contextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemOrder: 30 for: SpecCodeContext
]

{ #category : #activating }
SpecCodeInspectItCommand class >> shortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation 
		by: $i meta
		for: SpecCodeContext
]

{ #category : #accessing }
SpecCodeInspectItCommand >> defaultMenuItemName [

	^ 'Inspect it'
]

{ #category : #execution }
SpecCodeInspectItCommand >> execute [

	'OK' crLog
]

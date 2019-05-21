Class {
	#name : #SpecCodeDebugItCommand,
	#superclass : #SpecCodeSelectionCommand,
	#category : #'Spec-Commands-Code'
}

{ #category : #activating }
SpecCodeDebugItCommand class >> contextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemOrder: 40 for: SpecCodeContext
]

{ #category : #activating }
SpecCodeDebugItCommand class >> shortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation 
		by: $d shift meta
		for: SpecCodeContext
]

{ #category : #accessing }
SpecCodeDebugItCommand >> defaultMenuItemName [

	^ 'Debug it'
]

{ #category : #execution }
SpecCodeDebugItCommand >> execute [

	'OK' crLog
]

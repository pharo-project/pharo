Class {
	#name : #SpecCodeDoItCommand,
	#superclass : #SpecCodeSelectionCommand,
	#category : #'Spec-Commands-Code'
}

{ #category : #activating }
SpecCodeDoItCommand class >> contextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemOrder: 10 for: SpecCodeContext
]

{ #category : #activating }
SpecCodeDoItCommand class >> shortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation 
		by: $d meta
		for: SpecCodeContext
]

{ #category : #accessing }
SpecCodeDoItCommand >> defaultMenuItemName [

	^ 'Do it'
]

{ #category : #execution }
SpecCodeDoItCommand >> execute [

	'OK' crLog
]

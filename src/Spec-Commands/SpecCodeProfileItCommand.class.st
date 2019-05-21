Class {
	#name : #SpecCodeProfileItCommand,
	#superclass : #SpecCodeSelectionCommand,
	#category : #'Spec-Commands-Code'
}

{ #category : #activating }
SpecCodeProfileItCommand class >> contextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemOrder: 50 for: SpecCodeContext
]

{ #category : #accessing }
SpecCodeProfileItCommand >> defaultMenuItemName [

	^ 'Profile it'
]

{ #category : #execution }
SpecCodeProfileItCommand >> execute [

	'OK' crLog
]

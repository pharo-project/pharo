"
I'm a trait to add context menus capabilities to the presenter that use it.
Note that I just add some behaviour at presenter level, the right implementation needs 
to be solved in the backend adaptor.
"
Trait {
	#name : #SpTContextMenu,
	#instVars : [
		'#contextMenu => SpObservableSlot'
	],
	#category : #'Spec2-Core-Base'
}

{ #category : #accessing }
SpTContextMenu >> contextMenu [

	^ contextMenu
]

{ #category : #accessing }
SpTContextMenu >> contextMenu: aValuable [
	"Set the block used to define a context menu for the presenter"

	contextMenu := aValuable
]

{ #category : #'api-events' }
SpTContextMenu >> whenMenuChangedDo: aBlock [
	"Set a block to value when the menu block has changed"

	self property: #contextMenu whenChangedDo: aBlock
]

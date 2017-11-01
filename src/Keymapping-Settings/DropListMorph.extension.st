Extension { #name : #DropListMorph }

{ #category : #'*Keymapping-Settings' }
DropListMorph >> initializeShortcuts: aKMDispatcher [
	super initializeShortcuts: aKMDispatcher.
	aKMDispatcher attachCategory: #MorphFocusNavigation
]

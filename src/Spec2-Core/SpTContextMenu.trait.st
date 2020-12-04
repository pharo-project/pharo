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

{ #category : #api }
SpTContextMenu >> contextMenu [
	"Answer context menu or nil if there is none defined.
	 Context menu can be an instace of `SpMenuPresenter` or a block (that will answer eventually an 
	 instance of `SpMenuPresenter` aswell. 
	 If your context menu is defined through a block it means is dynamic, and it will be evaluated 
	 each time the context menu is invoked."

	^ contextMenu
]

{ #category : #api }
SpTContextMenu >> contextMenu: aValuableOrMenuPresenter [
	"Set the context menu.
	 Context menu can be an instace of `SpMenuPresenter` or a block (that will answer eventually an 
	 instance of `SpMenuPresenter` aswell. 
	 If your context menu is defined through a block it means is dynamic, and it will be evaluated 
	 each time the context menu is invoked."

	contextMenu := aValuableOrMenuPresenter
]

{ #category : #initialization }
SpTContextMenu >> initialize [
	
	self class initializeSlots: self.
	super initialize.
]

{ #category : #'api-events' }
SpTContextMenu >> whenMenuChangedDo: aBlock [
	"Inform when menu definition changed. 
	 `aBlock` receives zero arguments."

	self property: #contextMenu whenChangedDo: aBlock
]

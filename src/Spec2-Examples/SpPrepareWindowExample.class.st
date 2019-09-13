"
I'm an example showing how to use the #prepareWindow: message to enhance the window in which your presenter will be shown. 
The example adds a menu, a toolbar and a statusbar. 
You can choose which one(s) you want to add.

self show.
"
Class {
	#name : #SpPrepareWindowExample,
	#superclass : #SpPresenter,
	#instVars : [
		'menu',
		'toolBar',
		'statusBar',
		'text',
		'count'
	],
	#category : #'Spec2-Examples-Standalone'
}

{ #category : #specs }
SpPrepareWindowExample class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #text;
		yourself
]

{ #category : #showing }
SpPrepareWindowExample class >> show [
	<script>

	self new openWithSpec
]

{ #category : #'private building' }
SpPrepareWindowExample >> buildFileMenu [

	^ self newMenu
		addItem: [ :item | item name: 'Push message'; action: [ self pushMessage ] ];
		addItem: [ :item | item name: 'Pop message'; action: [ self popMessage ] ];
		yourself
]

{ #category : #'private building' }
SpPrepareWindowExample >> buildMenuBar [

	^ self newMenuBar
		addGroup: [ :group | group
			addItem: [ :item | item name: 'File'; subMenu: self buildFileMenu ] ]
]

{ #category : #'private building' }
SpPrepareWindowExample >> buildPopMessageToolBarItem [
		
	^ SpToolBarButton new
		label: 'Pop';
		icon: (self iconNamed: #glamorousDown);
		help: 'Pop last status bar message';
		action: [ self popMessage ];
		yourself
]

{ #category : #'private building' }
SpPrepareWindowExample >> buildPushMessageToolBarItem [
		
	^ SpToolBarButton new
		label: 'Push';
		icon: (self iconNamed: #glamorousUp);
		help: 'Push a new status bar message';
		action: [ self pushMessage ];
		yourself
]

{ #category : #'private building' }
SpPrepareWindowExample >> buildStatusBar [

	^ SpStatusBarPresenter new
]

{ #category : #'private building' }
SpPrepareWindowExample >> buildToolBar [
	
	^ self newToolBar 
		addItem: self buildPushMessageToolBarItem;
		addItem: self buildPopMessageToolBarItem;
		addItem: self buildPopMessageToolBarItem position: SpToolBarItemPosition right;
		yourself
]

{ #category : #api }
SpPrepareWindowExample >> initialExtent [
 
	^ 600@400
]

{ #category : #initialization }
SpPrepareWindowExample >> initialize [

	super initialize.
	count := 0
]

{ #category : #initialization }
SpPrepareWindowExample >> initializePresenters [
	
	text := self newText text: 'Implementing #prepareWindow:, you can add a menu, toolbar and statusBar to your component.

BEWARE: This elements will be added just when you will open the presenter as a window or dialog. Otherwise, your component will behave just as another composable presenter.'
]

{ #category : #initialization }
SpPrepareWindowExample >> initializeWindow: aWindowPresenter [
	"I'm called whenever the component will be displayed in a window (or dialog). 
	 This is usually attained sending #openWithSpec or #openDialogWithSpec."
	
	super initializeWindow: aWindowPresenter.
	aWindowPresenter
		menu: (menu := self buildMenuBar);
		toolBar: (toolBar := self buildToolBar);
		statusBar: (statusBar := self buildStatusBar)
]

{ #category : #actions }
SpPrepareWindowExample >> popMessage [

	statusBar popMessage
]

{ #category : #actions }
SpPrepareWindowExample >> pushMessage [
	
	count := count + 1.
	statusBar pushMessage: ('StatusBar message {1}...' format: { count })
]

{ #category : #'accessing ui' }
SpPrepareWindowExample >> text [ 

	^ text
]

{ #category : #actions }
SpPrepareWindowExample >> text: aPresenter [

	text := aPresenter
]

{ #category : #api }
SpPrepareWindowExample >> title [ 

	^ 'Example of using #prepareWindow:'
]

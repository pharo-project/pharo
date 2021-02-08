"
I'm an example showing how to use the #prepareWindow: message to enhance the window in which your presenter will be shown. 
The example adds a menu, a toolbar and a statusbar. 
You can choose which one(s) you want to add.

self show.
"
Class {
	#name : #SpInitializeWindowExample,
	#superclass : #SpPresenter,
	#instVars : [
		'menu',
		'statusBar',
		'text',
		'count',
		'toolbar'
	],
	#category : #'Spec2-Examples-Standalone'
}

{ #category : #specs }
SpInitializeWindowExample class >> defaultSpec [
	^ SpBoxLayout newTopToBottom
		add: #text;
		yourself
]

{ #category : #showing }
SpInitializeWindowExample class >> show [
	<script>

	self new openWithSpec
]

{ #category : #'private building' }
SpInitializeWindowExample >> buildFileMenu [

	^ self newMenu
		addItem: [ :item | item name: 'Push message'; action: [ self pushMessage ] ];
		addItem: [ :item | item name: 'Pop message'; action: [ self popMessage ] ];
		yourself
]

{ #category : #'private building' }
SpInitializeWindowExample >> buildMenuBar [

	^ self newMenuBar
		addGroup: [ :group | group
			addItem: [ :item | item name: 'File'; subMenu: self buildFileMenu ] ]
]

{ #category : #'private building' }
SpInitializeWindowExample >> buildPopMessageToolBarItem [
		
	^ SpToolbarButtonPresenter new
		label: 'Pop';
		icon: (self iconNamed: #glamorousDown);
		help: 'Pop last status bar message';
		action: [ self popMessage ];
		yourself
]

{ #category : #'private building' }
SpInitializeWindowExample >> buildPushMessageToolBarItem [
		
	^ SpToolbarButtonPresenter new
		label: 'Push';
		icon: (self iconNamed: #glamorousUp);
		help: 'Push a new status bar message';
		action: [ self pushMessage ];
		yourself
]

{ #category : #'private building' }
SpInitializeWindowExample >> buildStatusBar [

	^ SpStatusBarPresenter new
]

{ #category : #'private building' }
SpInitializeWindowExample >> buildToolbar [
	
	^ self newToolbar 
		addItem: self buildPushMessageToolBarItem;
		addItem: self buildPopMessageToolBarItem;
		addItem: self buildPopMessageToolBarItem position: SpToolbarItemPosition right;
		yourself
]

{ #category : #api }
SpInitializeWindowExample >> initialExtent [
 
	^ 600@400
]

{ #category : #initialization }
SpInitializeWindowExample >> initialize [

	super initialize.
	count := 0
]

{ #category : #initialization }
SpInitializeWindowExample >> initializePresenters [
	
	text := self newText text: 'Implementing #initializeWindow:, you can add a menu, toolbar and statusBar to your component.

BEWARE: This elements will be added just when you will open the presenter as a window or dialog. Otherwise, your component will behave just as another composable presenter.'
]

{ #category : #initialization }
SpInitializeWindowExample >> initializeWindow: aWindowPresenter [
	"I'm called whenever the component will be displayed in a window (or dialog). 
	 This is usually attained sending #openWithSpec or #openDialogWithSpec."
	
	super initializeWindow: aWindowPresenter.
	aWindowPresenter
		menu: (menu := self buildMenuBar);
		toolbar: (toolbar := self buildToolbar);
		statusBar: (statusBar := self buildStatusBar)
]

{ #category : #actions }
SpInitializeWindowExample >> popMessage [

	statusBar popMessage
]

{ #category : #actions }
SpInitializeWindowExample >> pushMessage [
	
	count := count + 1.
	statusBar pushMessage: ('StatusBar message {1}...' format: { count })
]

{ #category : #'accessing ui' }
SpInitializeWindowExample >> text [ 

	^ text
]

{ #category : #actions }
SpInitializeWindowExample >> text: aPresenter [

	text := aPresenter
]

{ #category : #api }
SpInitializeWindowExample >> title [ 

	^ 'Example of using #prepareWindow:'
]

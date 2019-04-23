"
I'm an example showing how to use the #prepareWindow: message to enhance the window in which your presenter will be shown. 
The example adds a menu, a toolbar and a statusbar. 
You can choose which one(s) you want to add.

self show.
"
Class {
	#name : #PrepareWindowExample,
	#superclass : #ComposablePresenter,
	#instVars : [
		'menu',
		'toolBar',
		'statusBar',
		'text',
		'count'
	],
	#category : #'Spec-Examples-Standalone'
}

{ #category : #specs }
PrepareWindowExample class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed 
		newColumn: #text
]

{ #category : #showing }
PrepareWindowExample class >> show [
	<script>

	self new openWithSpec
]

{ #category : #api }
PrepareWindowExample >> initialExtent [
 
	^ 600@400
]

{ #category : #initialization }
PrepareWindowExample >> initialize [

	super initialize.
	count := 0
]

{ #category : #initialization }
PrepareWindowExample >> initializeWidgets [
	
	text := self newText text: 'Implementing #prepareWindow:, you can add a menu, toolbar and statusBar to your component.

BEWARE: This elements will be added just when you will open the presenter as a window or dialog. Otherwise, your component will behave just as another composable presenter.'
]

{ #category : #initialization }
PrepareWindowExample >> initializeWindow: aWindowPresenter [
	"I'm called whenever the component will be displayed in a window (or dialog). 
	 This is usually attained sending #openWithSpec or #openDialogWithSpec."
	
	super initializeWindow: aWindowPresenter.
	aWindowPresenter
		menu: (menu := self newMenubar);
		toolBar: (toolBar := self newToolBar);
		statusBar: (statusBar := self newStatusBar)
]

{ #category : #'private factory' }
PrepareWindowExample >> newFileMenu [
	^ self newMenu
		addItem: [ :item | 
			item
				name: 'Push message';
				action: [ self pushMessage ] ];
		addItem: [ :item | 
			item
				name: 'Pop message';
				action: [ self popMessage ] ];
		yourself
]

{ #category : #'private factory' }
PrepareWindowExample >> newMenubar [
	^ self newMenuBar
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: 'File';
						subMenu: self newFileMenu ] ]
]

{ #category : #'private factory' }
PrepareWindowExample >> newPopMessageToolBarItem [
		
	^ ToolBarButton new
		label: 'Pop';
		icon: (self iconNamed: #glamorousDown);
		help: 'Pop last status bar message';
		action: [ self popMessage ];
		yourself
]

{ #category : #'private factory' }
PrepareWindowExample >> newPushMessageToolBarItem [
		
	^ ToolBarButton new
		label: 'Push';
		icon: (self iconNamed: #glamorousUp);
		help: 'Push a new status bar message';
		action: [ self pushMessage ];
		yourself
]

{ #category : #'private factory' }
PrepareWindowExample >> newStatusBar [

	^ StatusBarPresenter new
]

{ #category : #'private factory' }
PrepareWindowExample >> newToolBar [
	
	^ ToolBarPresenter new 
		addItem: self newPushMessageToolBarItem;
		addItem: self newPopMessageToolBarItem;
		addItem: self newPopMessageToolBarItem position: ToolBarItemPosition right;
		yourself
]

{ #category : #actions }
PrepareWindowExample >> popMessage [

	statusBar popMessage
]

{ #category : #actions }
PrepareWindowExample >> pushMessage [
	
	count := count + 1.
	statusBar pushMessage: ('StatusBar message {1}...' format: { count })
]

{ #category : #'accessing ui' }
PrepareWindowExample >> text [ 

	^ text
]

{ #category : #actions }
PrepareWindowExample >> text: aPresenter [

	text := aPresenter
]

{ #category : #api }
PrepareWindowExample >> title [ 

	^ 'Example of using #prepareWindow:'
]

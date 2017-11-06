"
A MethodToolbar is xxxxxxxxx.

self example
"
Class {
	#name : #MethodToolbar,
	#superclass : #ComposablePresenter,
	#instVars : [
		'methodHolder',
		'browseModel',
		'sendersModel',
		'implementorsModel',
		'versionModel',
		'dropListModel',
		'model',
		'usersModel'
	],
	#category : #Spec-PolyWidgets-Widgets
}

{ #category : #specs }
MethodToolbar class >> defaultSpec [
	<spec>
	^ SpecLayout composed
		newRow:
				[ :row | 
			row
				add: #(#model #browseModel);
				add: #(#model #usersModel);
				add: #(#model #sendersModel);
				add: #(#model #implementorsModel);
				add: #(#model #versionModel);
				add: #(#model #dropListModel) ];
		yourself
]

{ #category : #example }
MethodToolbar class >> example [
	| example |
	example := MethodToolbar new.
	example
		method: ButtonPresenter >> #state:;
		method: nil;
		extent: 550 @ 100;
		openWithSpec.
		^example 
]

{ #category : #specs }
MethodToolbar class >> title [

	^ 'Toolbar'
]

{ #category : #private }
MethodToolbar >> addAll: aWindow  withSpec: aSpec [

	aWindow
		addMorph: (self buildWithSpec: aSpec)
		fullFrame: (LayoutFrame identity bottomFraction: 0; bottomOffset: 30).
		
]

{ #category : #api }
MethodToolbar >> addItemLabeled: aString do: aBlock [

	dropListModel addItemLabeled: aString do: aBlock
]

{ #category : #private }
MethodToolbar >> browseMethod [

	methodHolder value
		ifNotNil: [:aMethod | aMethod browse ]
]

{ #category : #accessing }
MethodToolbar >> browseModel [

	^ browseModel
]

{ #category : #accessing }
MethodToolbar >> dropListModel [

	^ dropListModel
]

{ #category : #api }
MethodToolbar >> emptyDropList [
	
	dropListModel emptyList
]

{ #category : #private }
MethodToolbar >> implementorsMethod [

	methodHolder value
		ifNotNil: [:aMethod | self model browseMessagesFrom: aMethod selector ]
]

{ #category : #accessing }
MethodToolbar >> implementorsModel [

	^ implementorsModel
]

{ #category : #initialization }
MethodToolbar >> initialize [

	methodHolder := nil asValueHolder.
	model := AbstractTool new.
	
	super initialize.
]

{ #category : #initialization }
MethodToolbar >> initializeWidgets [
	browseModel := self newButton.
	usersModel := self newButton.
	sendersModel := self newButton.
	implementorsModel := self newButton.
	versionModel := self newButton.
	dropListModel := self newDropList.
	self
		setFocusOrder;
		setBrowseModel;
		setUsersModel;
		setVersionModel;
		setSendersModel;
		setImplementorsModel
]

{ #category : #api }
MethodToolbar >> method: aMethod [

	methodHolder value: aMethod
]

{ #category : #accessing }
MethodToolbar >> model [

	^ model
]

{ #category : #initialization }
MethodToolbar >> registerForChanges [
	methodHolder
		whenChangedDo:
			[ :contents | 
			| boolean |
			boolean := contents ifNil: [ false ] ifNotNil: [ true ].
			browseModel state: boolean.
			usersModel state: boolean.
			sendersModel state: boolean.
			versionModel state: boolean.
			implementorsModel state: boolean ]
]

{ #category : #private }
MethodToolbar >> sendersMethod [

	methodHolder value
		ifNotNil: [:aMethod | self model browseSendersOfMessagesFrom: aMethod selector ]
]

{ #category : #accessing }
MethodToolbar >> sendersModel [

	^ sendersModel
]

{ #category : #initialization }
MethodToolbar >> setBrowseModel [
	
	browseModel
		state: false;
		label: 'Browse';
		action: [ self browseMethod ].
	

]

{ #category : #initialization }
MethodToolbar >> setFocusOrder [

	self focusOrder
		add: browseModel;
		add: usersModel;
		add: sendersModel;
		add: implementorsModel;
		add: versionModel.
		
]

{ #category : #initialization }
MethodToolbar >> setImplementorsModel [
	
	implementorsModel
		state: false;
		label: 'Implementors';
		action: [ self implementorsMethod ].
	

]

{ #category : #initialization }
MethodToolbar >> setSendersModel [
	
	sendersModel
		state: false;
		label: 'Senders';
		action: [ self sendersMethod ].
	

]

{ #category : #initialization }
MethodToolbar >> setUsersModel [
	usersModel
		state: false;
		label: 'Users';
		action: [ self usersMethod ]
]

{ #category : #initialization }
MethodToolbar >> setVersionModel [
	
	versionModel
		state: false;
		label: 'Version';
		action: [ self versionMethod ].
	

]

{ #category : #private }
MethodToolbar >> usersMethod [
	methodHolder value methodClass ifNotNil: [ :class | self model browseClassRefsOf: class ]
]

{ #category : #accessing }
MethodToolbar >> usersModel [
	^ usersModel
]

{ #category : #private }
MethodToolbar >> versionMethod [

	methodHolder value
		ifNotNil: [:aMethod | self model browseVersionsFrom: aMethod ]
]

{ #category : #accessing }
MethodToolbar >> versionModel [

	^ versionModel
]

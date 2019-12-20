"
A MethodToolbar is xxxxxxxxx.

self example
"
Class {
	#name : #SpMethodToolbar,
	#superclass : #SpPresenter,
	#instVars : [
		'#browseModel',
		'#sendersModel',
		'#implementorsModel',
		'#versionModel',
		'#dropListModel',
		'#model',
		'#usersModel',
		'#method => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpMethodToolbar class >> defaultSpec [
	<spec>
	^ SpBoxLayout newHorizontal
		add: #browseModel;
		add: #usersModel;
		add: #sendersModel;
		add: #implementorsModel;
		add: #versionModel;
		add: #dropListModel;
		yourself
]

{ #category : #specs }
SpMethodToolbar class >> title [

	^ 'Toolbar'
]

{ #category : #api }
SpMethodToolbar >> addItemLabeled: aString do: aBlock [

	dropListModel addItemLabeled: aString do: aBlock
]

{ #category : #private }
SpMethodToolbar >> browseMethod [
	self method ifNotNil: [ :aMethod | aMethod browse ]
]

{ #category : #accessing }
SpMethodToolbar >> browseModel [

	^ browseModel
]

{ #category : #accessing }
SpMethodToolbar >> dropListModel [

	^ dropListModel
]

{ #category : #api }
SpMethodToolbar >> emptyDropList [
	
	dropListModel emptyList
]

{ #category : #private }
SpMethodToolbar >> implementorsMethod [
	self method ifNotNil: [ :aMethod | self model browseMessagesFrom: aMethod selector ]
]

{ #category : #accessing }
SpMethodToolbar >> implementorsModel [

	^ implementorsModel
]

{ #category : #initialization }
SpMethodToolbar >> initialize [
	model := AbstractTool new.

	super initialize
]

{ #category : #initialization }
SpMethodToolbar >> initializePresenters [
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

{ #category : #accessing }
SpMethodToolbar >> method [
	^ method
]

{ #category : #api }
SpMethodToolbar >> method: aMethod [
	method := aMethod
]

{ #category : #accessing }
SpMethodToolbar >> model [

	^ model
]

{ #category : #initialization }
SpMethodToolbar >> registerForChanges [
	self
		property: #method
		whenChangedDo: [ :contents | 
			| boolean |
			boolean := contents ifNil: [ false ] ifNotNil: [ true ].
			browseModel state: boolean.
			usersModel state: boolean.
			sendersModel state: boolean.
			versionModel state: boolean.
			implementorsModel state: boolean ]
]

{ #category : #private }
SpMethodToolbar >> sendersMethod [
	self method ifNotNil: [ :aMethod | self model browseSendersOfMessagesFrom: aMethod selector ]
]

{ #category : #accessing }
SpMethodToolbar >> sendersModel [

	^ sendersModel
]

{ #category : #initialization }
SpMethodToolbar >> setBrowseModel [
	
	browseModel
		state: false;
		label: 'Browse';
		action: [ self browseMethod ].
	

]

{ #category : #initialization }
SpMethodToolbar >> setFocusOrder [

	self focusOrder
		add: browseModel;
		add: usersModel;
		add: sendersModel;
		add: implementorsModel;
		add: versionModel.
		
]

{ #category : #initialization }
SpMethodToolbar >> setImplementorsModel [
	
	implementorsModel
		state: false;
		label: 'Implementors';
		action: [ self implementorsMethod ].
	

]

{ #category : #initialization }
SpMethodToolbar >> setSendersModel [
	
	sendersModel
		state: false;
		label: 'Senders';
		action: [ self sendersMethod ].
	

]

{ #category : #initialization }
SpMethodToolbar >> setUsersModel [
	usersModel
		state: false;
		label: 'Users';
		action: [ self usersMethod ]
]

{ #category : #initialization }
SpMethodToolbar >> setVersionModel [
	
	versionModel
		state: false;
		label: 'Version';
		action: [ self versionMethod ].
	

]

{ #category : #private }
SpMethodToolbar >> usersMethod [
	self method methodClass ifNotNil: [ :class | self model browseClassRefsOf: class ]
]

{ #category : #accessing }
SpMethodToolbar >> usersModel [
	^ usersModel
]

{ #category : #private }
SpMethodToolbar >> versionMethod [
	self method ifNotNil: [ :aMethod | self model browseVersionsFrom: aMethod ]
]

{ #category : #accessing }
SpMethodToolbar >> versionModel [

	^ versionModel
]

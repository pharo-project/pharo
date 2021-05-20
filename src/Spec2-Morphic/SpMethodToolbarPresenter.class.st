"
A MethodToolbar is xxxxxxxxx.

self example
"
Class {
	#name : #SpMethodToolbarPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'browseModel',
		'sendersModel',
		'implementorsModel',
		'versionModel',
		'dropListModel',
		'model',
		'usersModel',
		'method'
	],
	#category : #'Spec2-Morphic-Widgets'
}

{ #category : #specs }
SpMethodToolbarPresenter class >> defaultSpec [
	<spec>
	^ SpBoxLayout newLeftToRight
		add: #browseModel;
		add: #usersModel;
		add: #sendersModel;
		add: #implementorsModel;
		add: #versionModel;
		add: #dropListModel;
		yourself
]

{ #category : #specs }
SpMethodToolbarPresenter class >> title [

	^ 'Toolbar'
]

{ #category : #api }
SpMethodToolbarPresenter >> addItemLabeled: aString do: aBlock [

	dropListModel addItemLabeled: aString do: aBlock
]

{ #category : #private }
SpMethodToolbarPresenter >> browseMethod [

	self method ifNotNil: [ :aMethod | aMethod browse ]
]

{ #category : #accessing }
SpMethodToolbarPresenter >> browseModel [

	^ browseModel
]

{ #category : #accessing }
SpMethodToolbarPresenter >> dropListModel [

	^ dropListModel
]

{ #category : #api }
SpMethodToolbarPresenter >> emptyDropList [
	
	dropListModel emptyList
]

{ #category : #private }
SpMethodToolbarPresenter >> implementorsMethod [
	self method ifNotNil: [ :aMethod | self model browseMessagesFrom: aMethod selector ]
]

{ #category : #accessing }
SpMethodToolbarPresenter >> implementorsModel [

	^ implementorsModel
]

{ #category : #initialization }
SpMethodToolbarPresenter >> initialize [
	model := AbstractTool new.

	super initialize
]

{ #category : #initialization }
SpMethodToolbarPresenter >> initializePresenters [

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
SpMethodToolbarPresenter >> method [
	^ method
]

{ #category : #api }
SpMethodToolbarPresenter >> method: aMethod [

	method := aMethod
]

{ #category : #accessing }
SpMethodToolbarPresenter >> model [

	^ model
]

{ #category : #initialization }
SpMethodToolbarPresenter >> registerForChanges [
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
SpMethodToolbarPresenter >> sendersMethod [
	self method ifNotNil: [ :aMethod | self model browseSendersOfMessagesFrom: aMethod selector ]
]

{ #category : #accessing }
SpMethodToolbarPresenter >> sendersModel [

	^ sendersModel
]

{ #category : #initialization }
SpMethodToolbarPresenter >> setBrowseModel [
	
	browseModel
		state: false;
		label: 'Browse';
		action: [ self browseMethod ].
	

]

{ #category : #initialization }
SpMethodToolbarPresenter >> setFocusOrder [

	self focusOrder
		add: browseModel;
		add: usersModel;
		add: sendersModel;
		add: implementorsModel;
		add: versionModel.
		
]

{ #category : #initialization }
SpMethodToolbarPresenter >> setImplementorsModel [
	
	implementorsModel
		state: false;
		label: 'Implementors';
		action: [ self implementorsMethod ].
	

]

{ #category : #initialization }
SpMethodToolbarPresenter >> setModelBeforeInitialization: aMethod [

	self method: aMethod
]

{ #category : #initialization }
SpMethodToolbarPresenter >> setSendersModel [
	
	sendersModel
		state: false;
		label: 'Senders';
		action: [ self sendersMethod ].
	

]

{ #category : #initialization }
SpMethodToolbarPresenter >> setUsersModel [
	usersModel
		state: false;
		label: 'Users';
		action: [ self usersMethod ]
]

{ #category : #initialization }
SpMethodToolbarPresenter >> setVersionModel [
	
	versionModel
		state: false;
		label: 'Version';
		action: [ self versionMethod ].
	

]

{ #category : #private }
SpMethodToolbarPresenter >> usersMethod [
	self method methodClass ifNotNil: [ :class | self model browseClassRefsOf: class ]
]

{ #category : #accessing }
SpMethodToolbarPresenter >> usersModel [
	^ usersModel
]

{ #category : #private }
SpMethodToolbarPresenter >> versionMethod [
	self method ifNotNil: [ :aMethod | self model browseVersionsFrom: aMethod ]
]

{ #category : #accessing }
SpMethodToolbarPresenter >> versionModel [

	^ versionModel
]

"
I am the browser used to show a credentials store.
"
Class {
	#name : #IceTipCredentialsStoreBrowser,
	#superclass : #IceTipBrowser,
	#instVars : [
		'credentialsList'
	],
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #specs }
IceTipCredentialsStoreBrowser class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed 
		newColumn: [ :column |
			column 
				newRow: #toolbar height: IceTipToolbar defaultHeight;
				newRow: #credentialsList ]
]

{ #category : #accessing }
IceTipCredentialsStoreBrowser >> contextClass [
	^ IceTipCredentialsStoreContext 
]

{ #category : #'private factory' }
IceTipCredentialsStoreBrowser >> createCommandContextForSelection: selectedItems [
	^ self newContextWithSelection: selectedItems
]

{ #category : #accessing }
IceTipCredentialsStoreBrowser >> credentialSelected [
	| index dataSource |
	
	index := credentialsList widget selectedRowIndex.
	index = 0 ifTrue: [ ^ nil ].
	dataSource := credentialsList widget dataSource.
	dataSource hasElements ifFalse: [ ^ nil ].
	^ dataSource elementAt: index
]

{ #category : #accessing }
IceTipCredentialsStoreBrowser >> credentialsList [
	^ credentialsList
]

{ #category : #initialization }
IceTipCredentialsStoreBrowser >> initialize [

	model ifNil: [ model := IceCredentialStore current ].	
	super initialize.

]

{ #category : #initialization }
IceTipCredentialsStoreBrowser >> initializeCredentialsList [

	credentialsList widget 
		beResizable;
		addColumn: (IceTipTableColumn new 
			id: 'Host';
			action: #hostDescription;
			width: 100 * World displayScaleFactor;
			yourself);
		addColumn: (IceTipTableColumn new 
			id: 'Type';
			action: #type;
			width: 75 * World displayScaleFactor;
			yourself);
		addColumn: (IceTipTableColumn new 
			id: 'Description';
			action: #description;
			yourself);		
		dataSource: self newCredentialsDataSource;
		selectRowIndex: 1;
		enableFilter: IceTipRepositoryFilter;			
		explicitFunction
]

{ #category : #initialization }
IceTipCredentialsStoreBrowser >> initializeWidgets [
	super initializeWidgets.
	
	credentialsList := self newIceTable.
	self initializeCredentialsList.
	
	self focusOrder add: credentialsList 
]

{ #category : #'private factory' }
IceTipCredentialsStoreBrowser >> newContextWithSelection: anObject [ 

	^ IceTipCredentialContext new
		store: model;
		credential: anObject;
		tool: self;
		yourself
]

{ #category : #'private - datasource' }
IceTipCredentialsStoreBrowser >> newCredentialsDataSource [
	^ IceTipDataSource new
		tool: self;
		elements: self model allCredentials;
		yourself
]

{ #category : #accessing }
IceTipCredentialsStoreBrowser >> refresh [

	credentialsList widget
		dataSource: self newCredentialsDataSource;
		refresh
]

{ #category : #api }
IceTipCredentialsStoreBrowser >> title [
	^ model isCurrent
		ifTrue: [ 'Current Iceberg credentials' ]
		ifFalse: [ 'Credentials of ' , model printString ]
]

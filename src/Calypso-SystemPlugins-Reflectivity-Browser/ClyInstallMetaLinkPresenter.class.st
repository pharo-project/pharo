Class {
	#name : #ClyInstallMetaLinkPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'sourceNode',
		'list',
		'toolBar',
		'isForMetaLinkInstallation'
	],
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Metalinks'
}

{ #category : #specs }
ClyInstallMetaLinkPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		  add: #toolBar
		  expand: false
		  fill: false
		  padding: 0;
		  add: #list;
		  yourself
]

{ #category : #'instance creation' }
ClyInstallMetaLinkPresenter class >> onNode: aSourceNode forInstallation: forInstallation [
	^ self basicNew
		  sourceNode: aSourceNode;
		  isForMetaLinkInstallation: forInstallation;
		  initialize
]

{ #category : #opening }
ClyInstallMetaLinkPresenter class >> openInstallOnNode: aSourceNode [
	(self onNode: aSourceNode forInstallation: true) openWithSpec
]

{ #category : #opening }
ClyInstallMetaLinkPresenter class >> openUninstallOnNode: aSourceNode [
	(self onNode: aSourceNode forInstallation: false) openWithSpec
]

{ #category : #accessing }
ClyInstallMetaLinkPresenter >> allRelevantMetaLinks [
	| baseLinks |
	baseLinks := self isForMetaLinkInstallation
		             ifTrue: [ MetaLink allInstances ]
		             ifFalse: [ sourceNode links ].
	^ baseLinks reject: [ :link | 
		  link metaObject == Break or: [ 
			  { 
				  Watchpoint.
				  ExecutionCounter } includes: link metaObject class ] ]
]

{ #category : #initialization }
ClyInstallMetaLinkPresenter >> close [
	self withWindowDo: #close
]

{ #category : #initialization }
ClyInstallMetaLinkPresenter >> initializePresenter [
	list := self newList.
	list items: MetaLink allInstances.
	toolBar := self newToolBar.
	self toolbarButtons do: [ :button | toolBar addItem: button ]
]

{ #category : #installation }
ClyInstallMetaLinkPresenter >> installSelectedMetalink [
	list selection selectedItem ifNotNil: [ :metalink | 
		sourceNode link: metalink ].
	self close
]

{ #category : #testing }
ClyInstallMetaLinkPresenter >> isForMetaLinkInstallation [
	^isForMetaLinkInstallation ifNil:[isForMetaLinkInstallation := true]
]

{ #category : #accessing }
ClyInstallMetaLinkPresenter >> isForMetaLinkInstallation: anObject [
	isForMetaLinkInstallation := anObject
]

{ #category : #accessing }
ClyInstallMetaLinkPresenter >> sourceNode: anObject [
	sourceNode := anObject
]

{ #category : #accessing }
ClyInstallMetaLinkPresenter >> toolbarButtons [
	| buttons |
	buttons := OrderedCollection new.
	buttons add: (self isForMetaLinkInstallation
			 ifTrue: [ 
				 SpToolBarButton new
					 label: 'Install';
					 icon: (self iconNamed: #smallOk);
					 help: 'Install the selected metalink';
					 action: [ self installSelectedMetalink ];
					 yourself ]
			 ifFalse: [ 
				 SpToolBarButton new
					 label: 'Uninstall';
					 icon: (self iconNamed: #smallOk);
					 help: 'Uninstall the selected metalink';
					 action: [ self uninstallSelectedMetalink ];
					 yourself ]).
	buttons add: (SpToolBarButton new
			 label: 'Cancel';
			 icon: (self iconNamed: #smallCancel);
			 help: 'Cancel';
			 action: [ self close ];
			 yourself).
	^ buttons
]

{ #category : #installation }
ClyInstallMetaLinkPresenter >> uninstallSelectedMetalink [
	list selection selectedItem ifNotNil: [ :metalink | 
		sourceNode removeLink: metalink ].
	self close
]

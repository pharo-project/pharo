Class {
	#name : #ClyMetaLinkInstallationPresenter,
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
ClyMetaLinkInstallationPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		  add: #toolBar
		  expand: false
		  fill: false
		  padding: 0;
		  add: #list;
		  yourself
]

{ #category : #'instance creation' }
ClyMetaLinkInstallationPresenter class >> onNode: aSourceNode forInstallation: forInstallation [
	^ self basicNew
		  sourceNode: aSourceNode;
		  isForMetaLinkInstallation: forInstallation;
		  initialize
]

{ #category : #opening }
ClyMetaLinkInstallationPresenter class >> openInstallerOnNode: aSourceNode [
	(self onNode: aSourceNode forInstallation: true) openWithSpec
]

{ #category : #opening }
ClyMetaLinkInstallationPresenter class >> openUninstallerOnNode: aSourceNode [
	(self onNode: aSourceNode forInstallation: false) openWithSpec
]

{ #category : #accessing }
ClyMetaLinkInstallationPresenter >> allRelevantMetaLinks [
	| baseLinks |
	baseLinks := self isForMetaLinkInstallation
		             ifTrue: [ "This is temporary, I should have put a comment.
The idea would be to add an explicit option at the metalink level, so that only the ones that are ''installable through the menu'' would appear.
But we need to think about how to do that exactly." 
			             MetaLink allInstances ]
		             ifFalse: [ sourceNode links ].
	^ baseLinks reject: [ :link | 
		  link metaObject == Break or: [ 
			  { 
				  Watchpoint.
				  ExecutionCounter } includes: link metaObject class ] ]
]

{ #category : #initialization }
ClyMetaLinkInstallationPresenter >> close [
	self withWindowDo: #close
]

{ #category : #initialization }
ClyMetaLinkInstallationPresenter >> initializePresenter [
	list := self newList.
	list items: self allRelevantMetaLinks.
	toolBar := self newToolBar.
	self toolbarButtons do: [ :button | toolBar addItem: button ]
]

{ #category : #installation }
ClyMetaLinkInstallationPresenter >> installSelectedMetalink [
	list selection selectedItem ifNotNil: [ :metalink | 
		sourceNode link: metalink ].
	self close
]

{ #category : #testing }
ClyMetaLinkInstallationPresenter >> isForMetaLinkInstallation [
	^isForMetaLinkInstallation ifNil:[isForMetaLinkInstallation := true]
]

{ #category : #accessing }
ClyMetaLinkInstallationPresenter >> isForMetaLinkInstallation: anObject [
	isForMetaLinkInstallation := anObject
]

{ #category : #accessing }
ClyMetaLinkInstallationPresenter >> sourceNode: anObject [
	sourceNode := anObject
]

{ #category : #accessing }
ClyMetaLinkInstallationPresenter >> toolbarButtons [
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
ClyMetaLinkInstallationPresenter >> uninstallSelectedMetalink [
	list selection selectedItem ifNotNil: [ :metalink | 
		sourceNode removeLink: metalink ].
	self close
]

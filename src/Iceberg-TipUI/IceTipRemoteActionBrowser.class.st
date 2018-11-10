Class {
	#name : #IceTipRemoteActionBrowser,
	#superclass : #IceTipHistoryActionBrowser,
	#instVars : [
		'remotePanel'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #specs }
IceTipRemoteActionBrowser class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :mainColumn |
			mainColumn
				"newRow: #toolbar height: IceTipToolbar defaultHeight;"
				newRow: #remotePanel height: self inputTextHeight;
				newRow: [ :row |
					row newColumn: [ :column |
						column 
							add: #commitsPanel withSpec: #panelSpec;
							add: #actionPanel height: self iceActionPanelHeight ] ] ]
]

{ #category : #'instance creation' }
IceTipRemoteActionBrowser class >> onRepositoryModel: aRepository [
	^ self subclassResponsibility
]

{ #category : #initialization }
IceTipRemoteActionBrowser >> initializeWidgets [
	super initializeWidgets.
	
	remotePanel := self instantiate: IceTipSelectRemotePresenter on: self model. 
	remotePanel remoteList whenSelectedItemChanged: [ :remoteModel | 
		self moveToRemote: remoteModel  ].
		
	self focusOrder 
		add: self commitsPanel;
		add: self actionPanel;
		add: self remotePanel
]

{ #category : #'accessing ui' }
IceTipRemoteActionBrowser >> remotePanel [
	^ remotePanel
]

{ #category : #'accessing ui' }
IceTipRemoteActionBrowser >> remotePanel: anObject [
	remotePanel := anObject
]

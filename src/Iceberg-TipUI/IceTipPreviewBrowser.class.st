"
I'm a browser to show a preview of an operation that can be made right after.
Tipically, I'm using through my chldren, to do a checkout or merge.
"
Class {
	#name : #IceTipPreviewBrowser,
	#superclass : #IceTipBrowser,
	#instVars : [
		'diffPanel',
		'actionPanel',
		'button'
	],
	#category : #'Iceberg-TipUI-View-Branch'
}

{ #category : #specs }
IceTipPreviewBrowser class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :mainColumn |
			mainColumn
				"newRow: #toolbar height: IceTipToolbar defaultHeight;"
				newRow: [ :row |
					row newColumn: [ :column |
						column 
							add: #diffPanel;
							add: #actionPanel height: self iceActionPanelHeight ] ] ]
]

{ #category : #'accessing ui' }
IceTipPreviewBrowser >> actionPanel [
	^ actionPanel
]

{ #category : #'accessing ui' }
IceTipPreviewBrowser >> actionPanel: anObject [
	actionPanel := anObject
]

{ #category : #accessing }
IceTipPreviewBrowser >> contextClass [
	^ IceTipBranchContext
]

{ #category : #'accessing ui' }
IceTipPreviewBrowser >> diffPanel [
	^ diffPanel
]

{ #category : #'accessing ui' }
IceTipPreviewBrowser >> diffPanel: anObject [
	diffPanel := anObject
]

{ #category : #initialization }
IceTipPreviewBrowser >> initializeWidgets [
	super initializeWidgets.
	
	diffPanel := self instantiate: IceTipDiffPanel on: self model diff.
	actionPanel := self newIceActionPanel.
	actionPanel addAction: (button := self newActionButton).
	
	self whenBuiltDo: [ :ann | self addShortcutsTo: ann widget ].
	
	self focusOrder 
		add: self actionPanel;
		add: self diffPanel
]

{ #category : #'private factory' }
IceTipPreviewBrowser >> newActionButton [
	self subclassResponsibility
]

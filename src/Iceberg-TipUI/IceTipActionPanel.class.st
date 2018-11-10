"
I'm a simple action bar to be used as footer of certain panels (see IceTipCommentPanel as example)
"
Class {
	#name : #IceTipActionPanel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'panel',
		'actions',
		'actionMorphPanel',
		'actionWidth'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #accessing }
IceTipActionPanel class >> defaultActionWidth [
	^ 100 * World displayScaleFactor
]

{ #category : #accessing }
IceTipActionPanel class >> defaultHeight [
	^ 25 * World displayScaleFactor
]

{ #category : #specs }
IceTipActionPanel class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newRow: #panel
]

{ #category : #accessing }
IceTipActionPanel >> actionWidth [
	^ actionWidth ifNil: [ self class defaultActionWidth ]
]

{ #category : #initialization }
IceTipActionPanel >> actionWidth: aNumber [
	actionWidth := aNumber
]

{ #category : #accessing }
IceTipActionPanel >> addAction: aPresenter [
	actions add: aPresenter.
	self refresh
]

{ #category : #initialization }
IceTipActionPanel >> initialize [
	actions := OrderedCollection new.
	super initialize
]

{ #category : #initialization }
IceTipActionPanel >> initializeWidgets [
	panel := self newIcePanel.
	panel widget addMorphBack: (actionMorphPanel := PanelMorph new 
		changeTableLayout;
		listDirection: #rightToLeft;
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		yourself)
]

{ #category : #initialization }
IceTipActionPanel >> newIcePanel [
	^ super newIcePanel
		in: [ :this | this widget listDirection: #rightToLeft];
		yourself
]

{ #category : #'accessing ui' }
IceTipActionPanel >> panel [
	^ panel
]

{ #category : #'accessing ui' }
IceTipActionPanel >> panel: anObject [
	panel := anObject
]

{ #category : #initialization }
IceTipActionPanel >> refresh [
	actionMorphPanel removeAllMorphs.
	actions do: [ :each |
		actionMorphPanel addMorphBack: (each buildWithSpec
			hResizing: #rigid;
			width: self actionWidth;
			yourself) ]
]

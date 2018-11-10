"
I am a toolbar. 
I can create toolbars to be added to spec windows (usually on top).

You can instantiate it in a Spec component as is:

toolbar := self instantiate: IceTipToolbar.
toolbar displayModel: IceTipToolbarDisplayMode modeIconAndLabel.
toolbar addItem: (IceTipToolbarItem new
	label: 'Test';
	icon: #add asIcon;
	position: IceTipToolbarItemPosition left;
	yourself)
"
Class {
	#name : #IceTipToolbar,
	#superclass : #IceTipItemBar,
	#instVars : [
		'toolbarItemFont',
		'displayMode'
	],
	#category : #'Iceberg-TipUI-Spec-ItemBar'
}

{ #category : #accessing }
IceTipToolbar class >> defaultHeight [
	^ self defaultMode height * World displayScaleFactor
]

{ #category : #accessing }
IceTipToolbar class >> defaultItemSeparation [
	^ 0
]

{ #category : #accessing }
IceTipToolbar class >> defaultItemSize [
	^ 50 * World displayScaleFactor
]

{ #category : #accessing }
IceTipToolbar class >> defaultMode [
	^ IceTipToolbarDisplayMode default
]

{ #category : #specs }
IceTipToolbar class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed
		newRow: #panel
]

{ #category : #accessing }
IceTipToolbar >> addItemsFromContext: aContext [
	| menu |
	
	menu := CmdMenu activatedBy: IceTipToolbarActivation.
	menu buildInContext: aContext.
	menu buildIceToolbarOn: self

]

{ #category : #accessing }
IceTipToolbar >> displayMode [
	^ displayMode ifNil: [ displayMode := IceTipToolbarDisplayMode default ]
]

{ #category : #accessing }
IceTipToolbar >> displayMode: anObject [
	displayMode := anObject
]

{ #category : #'private factory' }
IceTipToolbar >> newToolbarItemFont [
	| buttonFont |
	buttonFont := Smalltalk ui theme buttonFont.
	^ LogicalFont 
		familyName: buttonFont familyName 
		pointSize: buttonFont pointSize - 3
]

{ #category : #accessing }
IceTipToolbar >> refresh [
	self leftPanel removeAllMorphs.
	self rightPanel removeAllMorphs.
	items do: [ :each |  
		each position 
			addItem: (each newToolbarButtonOn: self) 
			into: self ]
]

{ #category : #accessing }
IceTipToolbar >> toolbarItemFont [
	^ toolbarItemFont ifNil: [ toolbarItemFont := self newToolbarItemFont ]
]

{ #category : #accessing }
IceTipToolbar >> toolbarItemFont: anObject [
	toolbarItemFont := anObject
]

{ #category : #accessing }
IceTipToolbar >> toolbarItemSize [
	^ self class defaultItemSize
]

"
I am a morph responsible of showing a link like label. 
I can handle a block to act and showing the corresponding cursor when mouse enters and leave.
"
Class {
	#name : #GTLinkLabelMorph,
	#superclass : #LabelMorph,
	#instVars : [
		'onClickBlock'
	],
	#category : #'GT-Spotter-UI-Widgets'
}

{ #category : #initialization }
GTLinkLabelMorph >> execute [
	onClickBlock ifNotNil: [ onClickBlock cull: self ]
]

{ #category : #initialization }
GTLinkLabelMorph >> initWithContents: aString font: aFont emphasis: textEmphasis [
	super initWithContents: aString font: aFont emphasis: textEmphasis.
	self
		addEmphasis: TextEmphasis underlined;
		on: #click send: #execute to: self;
		on: #mouseEnter send: #mouseEnter: to: self;
		on: #mouseLeave send: #mouseLeave: to: self.
	
]

{ #category : #'event handling' }
GTLinkLabelMorph >> mouseEnter: evt [
	self currentHand showTemporaryCursor: Cursor webLink.

]

{ #category : #'event handling' }
GTLinkLabelMorph >> mouseLeave: evt [
	self currentHand showTemporaryCursor: nil

]

{ #category : #initialization }
GTLinkLabelMorph >> onClick: aBlock [
	onClickBlock := aBlock
]

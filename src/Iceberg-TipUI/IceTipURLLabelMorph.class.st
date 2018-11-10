Class {
	#name : #IceTipURLLabelMorph,
	#superclass : #LabelMorph,
	#instVars : [
		'onClickBlock'
	],
	#category : #'Iceberg-TipUI-Spec-ItemBar'
}

{ #category : #initialization }
IceTipURLLabelMorph >> execute [
	onClickBlock ifNotNil: [ onClickBlock cull: self ]
]

{ #category : #initialization }
IceTipURLLabelMorph >> initWithContents: aString font: aFont emphasis: textEmphasis [
	super initWithContents: aString font: aFont emphasis: textEmphasis.
	self
		addEmphasis: TextEmphasis underlined;
		on: #click send: #execute to: self;
		on: #mouseEnter send: #mouseEnter: to: self;
		on: #mouseLeave send: #mouseLeave: to: self.
	
]

{ #category : #'event handling' }
IceTipURLLabelMorph >> mouseEnter: evt [
	self currentHand showTemporaryCursor: Cursor webLink.

]

{ #category : #'event handling' }
IceTipURLLabelMorph >> mouseLeave: evt [
	self currentHand showTemporaryCursor: nil

]

{ #category : #initialization }
IceTipURLLabelMorph >> onClick: aBlock [
	onClickBlock := aBlock
]

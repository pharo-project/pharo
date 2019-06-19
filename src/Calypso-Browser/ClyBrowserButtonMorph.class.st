"
I am a toolbar button which executes particular command
"
Class {
	#name : #ClyBrowserButtonMorph,
	#superclass : #ClyBrowserToolbarItemMorph,
	#category : #'Calypso-Browser-Toolbar'
}

{ #category : #initialization }
ClyBrowserButtonMorph >> build [
	| label |
	label := self theme newLabelIn: self label: self title.
	label on: #click send: #executeCommand to: self.
	label emphasis: TextEmphasis underlined emphasisCode.
	
	self addMorph: label
]

{ #category : #operations }
ClyBrowserButtonMorph >> executeCommand [

	[ super executeCommand ]	on: Exception fork: [:exc | exc pass ]
]

{ #category : #installing }
ClyBrowserButtonMorph >> handlesMouseOver: evt [
	^true
]

{ #category : #initialization }
ClyBrowserButtonMorph >> initialize [
	super initialize.
	self 
		layoutInset: 4@0
]

{ #category : #installing }
ClyBrowserButtonMorph >> mouseEnter: evt [
	super mouseEnter: evt.
	self currentHand showTemporaryCursor: Cursor webLink.

]

{ #category : #installing }
ClyBrowserButtonMorph >> mouseLeave: evt [
	super mouseLeave: evt.
	self currentHand showTemporaryCursor: nil

]

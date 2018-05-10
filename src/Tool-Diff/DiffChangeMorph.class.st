Class {
	#name : #DiffChangeMorph,
	#superclass : #ComposableMorph,
	#instVars : [
		'diffMorph',
		'descriptionMorph'
	],
	#category : #'Tool-Diff'
}

{ #category : #'instance creation' }
DiffChangeMorph class >> from: old label: oldLabel to: new label: newLabel contextClass: aClass [
	"Answer a new instance of the receiver with the given
	old and new text and descriptions."

	^self new
		from: old
		to: new
		contextClass: aClass;
		fromDescription: oldLabel;
		toDescription: newLabel
]

{ #category : #actions }
DiffChangeMorph >> adoptPaneColor: paneColor [
	"Change the fill styles on the descriptions."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self descriptionMorph borderStyle: (self theme listNormalBorderStyleFor: self).
	self updateDescriptionFillStyle: paneColor
	
]

{ #category : #initialization }
DiffChangeMorph >> buildUI [

	|descriptionHeight|

	self
		diffMorph: self newDiffMorph;
		descriptionMorph: self newDescriptionMorph.
	descriptionHeight := self descriptionMorph minExtent y.
	self
		changeProportionalLayout;
		addMorph: self descriptionMorph
		fullFrame: ((0@0 corner: 1@0) asLayoutFrame bottomOffset: descriptionHeight);
		addMorph: self diffMorph
		fullFrame: (LayoutFrame identity topOffset: descriptionHeight).
	self
		extent: self initialExtent;
		updateDescriptionFillStyle: self paneColor
]

{ #category : #accessing }
DiffChangeMorph >> defaultTitle [
	"Answer the default title label for the receiver."

	^'Change Diff' translated
]

{ #category : #accessing }
DiffChangeMorph >> descriptionMorph [
	"Answer the value of descriptionMorph"

	^ descriptionMorph
]

{ #category : #accessing }
DiffChangeMorph >> descriptionMorph: anObject [
	"Set the value of descriptionMorph"

	descriptionMorph := anObject
]

{ #category : #accessing }
DiffChangeMorph >> diffMorph [
	"Answer the value of diffMorph"

	^ diffMorph
]

{ #category : #accessing }
DiffChangeMorph >> diffMorph: anObject [
	"Set the value of diffMorph"

	diffMorph := anObject
]

{ #category : #hooks }
DiffChangeMorph >> diffMorphClass [
	"Answer a the class to use for a new diff morph."

	^DiffMorph
]

{ #category : #actions }
DiffChangeMorph >> from: old to: new contextClass: aClass [
	"Set the old (src) and new (dst) text."

	self diffMorph
		from: old
		to: new
		contextClass: aClass
]

{ #category : #actions }
DiffChangeMorph >> fromDescription: aString [
	"Set the description for the left-hand side of the patch,
	typically 'currently in image'."

	self fromDescriptionMorph contents: aString
]

{ #category : #actions }
DiffChangeMorph >> fromDescriptionMorph [
	"Answer the morph for the source description."

	^self descriptionMorph firstSubmorph firstSubmorph
]

{ #category : #'user interface' }
DiffChangeMorph >> initialExtent [
	"Answer the initial extent for the receiver."

	^RealEstateAgent standardWindowExtent
]

{ #category : #initialization }
DiffChangeMorph >> initialize [
	super initialize.
	self buildUI
]

{ #category : #actions }
DiffChangeMorph >> newDescriptionMorph [
	"Answer a new morph for the descriptions of the source and destination."

	^(self newColumn: {
		(self newRow: {self newLabel: 'Original'}) layoutInset: 1.
		(self newRow: {self newLabel: 'Changed'})
			layoutInset: 1;
			listCentering: #bottomRight})
		layoutInset: 0;
		cellInset: 2;
		"fillStyle: Color white;"
		borderStyle: (self theme listNormalBorderStyleFor: self)
		
]

{ #category : #actions }
DiffChangeMorph >> newDiffMorph [
	"Answer a new morph for the source difference."

	^self diffMorphClass new
		borderStyle: (BorderStyle inset width: 1);
		addDependent: self;
		yourself
]

{ #category : #actions }
DiffChangeMorph >> toDescription: aString [
	"Set the description for the right-hand side of the patch,
	typically 'incoming' or 'changed'."

	self toDescriptionMorph contents: aString
]

{ #category : #actions }
DiffChangeMorph >> toDescriptionMorph [
	"Answer the morph for the destination description."

	^self descriptionMorph lastSubmorph firstSubmorph
]

{ #category : #actions }
DiffChangeMorph >> updateDescriptionFillStyle: aColor [
	"Change the colours on the descriptions."
	
	self descriptionMorph fillStyle: aColor.
	self fromDescriptionMorph owner color: (aColor alpha: 0.5).
	self toDescriptionMorph owner color: (aColor alpha: 0.3).
]

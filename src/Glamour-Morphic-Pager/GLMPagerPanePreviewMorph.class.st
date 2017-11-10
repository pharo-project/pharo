"
I am a morph showing a preview image of another morph.  I should be used like a singleton: my class provides an unique instance of me. That instance can be hiden, shown or updated.
"
Class {
	#name : #GLMPagerPanePreviewMorph,
	#superclass : #ImageMorph,
	#instVars : [
		'thumbnail',
		'position'
	],
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #Glamour-Morphic-Pager-Morphic
}

{ #category : #cleanup }
GLMPagerPanePreviewMorph class >> cleanUp [
	super cleanUp.
	uniqueInstance := nil
]

{ #category : #actions }
GLMPagerPanePreviewMorph class >> remove [

	self uniqueInstance delete.	
	uniqueInstance := nil.
]

{ #category : #accessing }
GLMPagerPanePreviewMorph class >> uniqueInstance [
	^ uniqueInstance ifNil: [ uniqueInstance := GLMPagerPanePreviewMorph new ].
]

{ #category : #actions }
GLMPagerPanePreviewMorph >> align: aPoint [
	
	position := aPoint.
]

{ #category : #initialization }
GLMPagerPanePreviewMorph >> defaultBorderWidth [
	^ 5
]

{ #category : #initialization }
GLMPagerPanePreviewMorph >> defaultBottomMargin [
	^ 11
]

{ #category : #actions }
GLMPagerPanePreviewMorph >> hideFromWorld [
	"Remove the morph from the world"
	self delete
]

{ #category : #initialization }
GLMPagerPanePreviewMorph >> initialize [
	super initialize.

	self color: Color transparent.
	
	self theme pagerWidgetThemer previewPaneStyleFor: self.
	self borderWidth: self defaultBorderWidth.
]

{ #category : #initialization }
GLMPagerPanePreviewMorph >> maxExtent [
	^ 300@300.
]

{ #category : #actions }
GLMPagerPanePreviewMorph >> showAt: aPoint [

	self 
		openInWorld;
		align: aPoint.
]

{ #category : #actions }
GLMPagerPanePreviewMorph >> updateFor: aMorph [

	thumbnail := aMorph taskThumbnailOfSize: self maxExtent.
	self image: thumbnail form.
	
	self position: position - ((thumbnail width / 2)@(thumbnail height + self defaultBottomMargin + self defaultBorderWidth)).
]

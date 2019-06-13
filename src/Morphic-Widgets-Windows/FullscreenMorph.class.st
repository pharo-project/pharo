"
Provides for another morph to occupy the full screen area (less docking bars).
"
Class {
	#name : #FullscreenMorph,
	#superclass : #Morph,
	#category : #'Morphic-Widgets-Windows-Widgets'
}

{ #category : #accessing }
FullscreenMorph >> contentMorph [
	"Answer the current content."
	
	^self submorphs first
]

{ #category : #initialize }
FullscreenMorph >> defaultColor [
	"Return the receiver's default color."
	
	^Color white
]

{ #category : #'change reporting' }
FullscreenMorph >> displayExtentChanged [

	self updateBounds
]

{ #category : #initialization }
FullscreenMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		changeProportionalLayout;
		bounds: self currentWorld clearArea;
		beSticky
]

{ #category : #'private - layout' }
FullscreenMorph >> layoutChanged [
	"Don't pass to owner, since the receiver doesn't care! Improves frame rate."
	
	fullBounds := nil.
	self layoutPolicy ifNotNil:[:l | l flushLayoutCache].
]

{ #category : #'open/close' }
FullscreenMorph >> openAsIs [
	"Open in the current world with the current position and extent."
	
	^self openAsIsIn: self currentWorld

]

{ #category : #'open/close' }
FullscreenMorph >> openAsIsIn: aWorld [
	"Start stepping."
	
	aWorld addMorph: self.
	(self submorphs notEmpty and: [self submorphs first isSystemWindow])
		ifTrue: [self submorphs first openedFullscreen].
	aWorld startSteppingSubmorphsOf: self.
	
	self announceOpened.
]

{ #category : #initialize }
FullscreenMorph >> openInWorld: aWorld [
	"Open as is."
	
	^self openAsIsIn: aWorld
]

{ #category : #accessing }
FullscreenMorph >> setContentMorph: aMorph [
	"Replace the submorphs with aMorph."
	
	self removeAllMorphs.
	self
		addMorph: aMorph
		fullFrame: LayoutFrame identity
]

{ #category : #'private - layout' }
FullscreenMorph >> updateBounds [
	"Update the receiver's bounds to fill the world."

	self bounds: self owner clearArea
	
]

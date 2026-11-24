Extension { #name : 'Morph' }

{ #category : '*Morphic-Widgets-Windows' }
Morph >> activate [
	"Mark the receiver and submorphs as active (foreground)."

	self submorphsDo: [:m | m activate]
]

{ #category : '*Morphic-Widgets-Windows' }
Morph >> collapse [
	CollapsedMorph new beReplacementFor: self
]

{ #category : '*Morphic-Widgets-Windows' }
Morph >> containingWindow [
	"Answer a window that contains the receiver"

	^ self ownerThatIsA: SystemWindow
]

{ #category : '*Morphic-Widgets-Windows' }
Morph >> dialogWindow [
	"Answer the receiver's dialog window."

	^self ownerThatIsA: DialogWindowMorph
]

{ #category : '*Morphic-Widgets-Windows' }
Morph >> embedInWindow [

	| window worldToUse |

	worldToUse := self world.		"I'm assuming we are already in a world"
	window := (SystemWindow labelled: self defaultLabel) model: nil.
	window bounds: ((self position - ((0@window labelHeight) + window borderWidth))
						corner: self bottomRight + window borderWidth).
	window addMorph: self frame: (0@0 extent: 1@1).
	window updatePaneColors.
	worldToUse addMorph: window.
	window activate
]

{ #category : '*Morphic-Widgets-Windows' }
Morph >> embeddedInMorphicWindowLabeled: labelString [
	| window |
	window := (SystemWindow labelled: labelString) model: nil.
	window setStripeColorsFrom:  (self theme windowColorFor: self).
	window addMorph: self frame: (0@0 extent: 1@1).
	^ window
]

{ #category : '*Morphic-Widgets-Windows' }
Morph >> isWindowActive: aSystemWindow [
	"Answer whether the given window is active.
	Implement in morphs that are designed to embed system windows.
	Delegate to owner."

	^self owner ifNil: [true] ifNotNil: [:o | o isWindowActive: aSystemWindow]
]

{ #category : '*Morphic-Widgets-Windows' }
Morph >> openInWindow [

	^self openInWindowLabeled: self defaultLabel
]

{ #category : '*Morphic-Widgets-Windows' }
Morph >> openInWindowLabeled: aString [

	^self openInWindowLabeled: aString inWorld: self currentWorld
]

{ #category : '*Morphic-Widgets-Windows' }
Morph >> openInWindowLabeled: aString inWorld: aWorld [
	"Changed to include the inset margin in the bound calculation."

	^ self wrappedInWindow
		setLabel: aString;
		openInWorld: aWorld
]

{ #category : '*Morphic-Widgets-Windows' }
Morph >> window [
	"Answer the receiver's window."

	^self ownerThatIsA: SystemWindow
]

{ #category : '*Morphic-Widgets-Windows' }
Morph >> wrappedInWindow [
	"Changed to include the inset margin in the bound calculation."

	| window extent |
	window := SystemWindow new.
	window
		addMorph: self frame: (0@0 extent: 1@1);
		updatePaneColors.
	" calculate extent after adding in case any size related attributes were changed.  Use
	fullBounds in order to trigger re-layout of layout morphs"
	extent := self fullBounds extent +
			(window borderWidth@window labelHeight) + window borderWidth +
			(window class borderWidth * 2 @ (window class borderWidth + 1)). "include inset margin"
	window extent: extent.
	^ window
]

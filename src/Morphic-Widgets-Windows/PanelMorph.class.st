"
A container morph that tracks the owner's pane colour unless an explicit fillStyle is specified. Additionally allows hooking of mouseOver events (no button down).
"
Class {
	#name : #PanelMorph,
	#superclass : #BorderedMorph,
	#category : #'Morphic-Widgets-Windows-Widgets'
}

{ #category : #accessing }
PanelMorph >> adoptPaneColor: paneColor [
	"Change our color too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self assureExtension.
	extension fillStyle ifNil: [self color: paneColor].
	self borderStyle baseColor: paneColor darker
]

{ #category : #protocol }
PanelMorph >> checkMorphsLeftOrTopFrom: splitter [
	| index size |
	
	size := submorphs size.
	index := submorphs identityIndexOf: splitter.
	
	index := index + 1.
	
	[ index <= size ] whileTrue: [ 
		| submorph done |
		submorph := submorphs at: index.
		splitter addLeftOrTop: submorph.
		done := self isResizeableMorph: submorph forSplitter: splitter.
		done
			ifTrue: [ index := size + 1 ].
		index := index + 1 ]
]

{ #category : #protocol }
PanelMorph >> checkMorphsLeftOrTopFrom: splitter size: size [
	| index |
	
	index := submorphs identityIndexOf: splitter.
	
	index := index + 1.
	
	[ index <= size ] whileTrue: [ 
		| submorph done |
		submorph := submorphs at: index.
		splitter addLeftOrTop: submorph.
		done := self isResizeableMorph: submorph forSplitter: splitter.
		done
			ifTrue: [ index := size + 1 ].
		index := index + 1 ]
]

{ #category : #protocol }
PanelMorph >> checkMorphsRightOrBottomFrom: splitter [
	| index |
	
	index := submorphs identityIndexOf: splitter.
	index := index - 1.
	
	[ index > 0 ] whileTrue: [ | submorph done |
		submorph := submorphs at: index.
		splitter addRightOrBottom: submorph.
		done := self isResizeableMorph: submorph forSplitter: splitter.
		done ifTrue: [ index := 0 ].
		index := index - 1 ]
]

{ #category : #protocol }
PanelMorph >> checkSplitters [
		
	(self submorphsSatisfying: [ :e | e isKindOf: ProportionalSplitterMorph ])
		do: [ :splitter |
			self checkMorphsLeftOrTopFrom: splitter.
			self checkMorphsRightOrBottomFrom: splitter ]
]

{ #category : #initialization }
PanelMorph >> defaultBorderWidth [
	"Answer the default border width for the receiver."
	
	^0
]

{ #category : #initialize }
PanelMorph >> defaultColor [
	"Answer the default color for the receiver."

	^Color transparent
]

{ #category : #protocol }
PanelMorph >> enabled: aBoolean [
	"Pass on to submorphs."

	self submorphsDo: [:m | m enabled: aBoolean]
]

{ #category : #'events-processing' }
PanelMorph >> handleMouseOver: anEvent [
	"System level event handling."
	
	super handleMouseOver: anEvent.
	(self handlesMouseOver: anEvent) ifTrue:[
		anEvent wasHandled: true.
		self mouseOver: anEvent]
]

{ #category : #initialization }
PanelMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		clipSubmorphs: true;
		beSticky "stop being grabbed"
]

{ #category : #protocol }
PanelMorph >> interactionState: aSymbol [
	"Pass on to submorphs."

	self submorphsDo: [:m |
		(m respondsTo: #interactionState:) ifTrue: [
			m interactionState: aSymbol]]
]

{ #category : #protocol }
PanelMorph >> isResizeableMorph: submorph forSplitter: e [
	| done |
	done := e splitsTopAndBottom
		ifTrue: [ submorph layoutFrame isVerticallyResizeable ]
		ifFalse: [ submorph layoutFrame isHorizontallyResizeable ].
	^ done
]

{ #category : #protocol }
PanelMorph >> mouseOver: anEvent [
	"Handle a mouseOver event, meaning the mouse just moved within the receiver
	with no button pressed. The default response is to let my eventHandler, if any, handle it."

	self eventHandler ifNotNil:
		[self eventHandler mouseOver: anEvent fromMorph: self]
]

{ #category : #protocol }
PanelMorph >> newHSplitter [

	^ ProportionalSplitterMorph new
			beSplitsTopAndBottom;
			setProperty: #model toValue: self.
]

{ #category : #protocol }
PanelMorph >> newHSplitterAt: anIndex [

	| above frame splitter index size |
	size := submorphs size.
	index := size - anIndex + 2.
	above := self submorphs at: index.
	frame := above layoutFrame.
	splitter := self newHSplitter.
	splitter  layoutFrame:  ((frame leftFraction @ frame bottomFraction corner: frame rightFraction @ frame bottomFraction) asLayoutFrame 
										bottomOffset: 4).			
	self privateAddMorph: splitter atIndex: (size - anIndex + 2)
]

{ #category : #protocol }
PanelMorph >> newVSplitter [

	^ ProportionalSplitterMorph new
			setProperty: #model toValue: self.
]

{ #category : #protocol }
PanelMorph >> newVSplitterAt: anIndex [
	| left frame splitter index size |

	size := submorphs size.
	index := size - anIndex + 2.
	left := self submorphs at: index.
	frame := left layoutFrame.
	
	splitter := self newVSplitter.
	splitter 
		layoutFrame: ((frame rightFraction @ frame topFraction corner: frame rightFraction @ frame bottomFraction) asLayoutFrame
							 rightOffset: 4).

	self privateAddMorph: splitter atIndex: (size - anIndex + 2)
]

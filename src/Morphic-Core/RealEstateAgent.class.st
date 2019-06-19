"
Responsible for real-estate management on the screen, which is to say, controlling where new windows appear, with what sizes, etc.  5/20/96 sw
"
Class {
	#name : #RealEstateAgent,
	#superclass : #Object,
	#classVars : [
		'StaggerOffset',
		'StandardSize',
		'UsedStrategy'
	],
	#category : #'Morphic-Core-Worlds'
}

{ #category : #utilities }
RealEstateAgent class >> assignCollapseFrameFor: aSSView [ 
	"Offer up a location along the left edge of the screen for a collapsed
	SSView. Make sure it doesn't overlap any other collapsed frames."
	| grid otherFrames topLeft viewBox collapsedFrame extent newFrame verticalBorderDistance top |
	grid := 8.
	verticalBorderDistance := 8.
	otherFrames := ( aSSView world windowsSatisfying: [:w | w ~= aSSView])
				collect: [:w | w collapsedFrame]
				thenSelect: [:rect | rect notNil].
	viewBox := aSSView world viewBox.
	collapsedFrame := aSSView collapsedFrame.
	extent := collapsedFrame notNil
				ifTrue: [collapsedFrame extent]
				ifFalse: [aSSView getRawLabel width + aSSView labelWidgetAllowance @ (aSSView labelHeight + 2)].
	collapsedFrame notNil
		ifTrue: [(otherFrames
					anySatisfy: [:f | collapsedFrame intersects: f])
				ifFalse: ["non overlapping"
					^ collapsedFrame]].
	top := viewBox top + verticalBorderDistance.
	[topLeft := viewBox left @ top.
	newFrame := topLeft extent: extent.
	newFrame bottom <= (viewBox height - verticalBorderDistance)]
		whileTrue: [(otherFrames
					anySatisfy: [:w | newFrame intersects: w])
				ifFalse: ["no overlap"
					^ newFrame].
			top := top + grid].
	"If all else fails... (really to many wins here)"
	^ 0 @ 0 extent: extent
]

{ #category : #utilities }
RealEstateAgent class >> assignCollapsePointFor: aSSView [ 
	"Offer up a location along the left edge of the screen for a collapsed
	SSView. Make sure it doesn't overlap any other collapsed frames."
	| grid otherFrames y free topLeft viewBox |
	grid := 24.
	"should be mult of 8, since manual move is gridded by 8"
	otherFrames := ( aSSView world   windowsSatisfying: [:w | true])
				collect: [:w | w collapsedFrame]
				thenSelect: [:rect | rect notNil].
	viewBox := aSSView world viewBox.
	y := viewBox top.
	[(y := y + grid) <= (viewBox height - grid)]
		whileTrue: [topLeft := viewBox left @ y.
			free := true.
			otherFrames
				do: [:w | free := free & (topLeft ~= w topLeft)].
			free
				ifTrue: [^ topLeft]].
	"If all else fails..."
	^ 0 @ 0
]

{ #category : #strategy }
RealEstateAgent class >> cascadeFor: aView initialExtent: initialExtent world: aWorld [

	| position allowedArea |
	allowedArea := self maximumUsableAreaInWorld: aWorld.
	position := aWorld currentWindow isMorph 
		ifFalse: [ aWorld center - (initialExtent / 2)]
		ifTrue: [ aWorld currentWindow position + 20].
	^ (position extent: initialExtent)
		translatedAndSquishedToBeWithin: allowedArea
]

{ #category : #framing }
RealEstateAgent class >> initialFrameFor: aView initialExtent: initialExtent world: aWorld [ 
	"Find a plausible initial screen area for the supplied view, which
	should be a StandardSystemView, taking into account the
	'reverseWindowStagger' Preference, the size needed, and other
	windows currently on the screen."
	
	^self perform: self usedStrategy with: aView with: initialExtent with: aWorld
	
]

{ #category : #framing }
RealEstateAgent class >> initialFrameFor: aView world: aWorld [
	"Find a plausible initial screen area for the supplied view.  See called method."

	^ self initialFrameFor: aView initialExtent: aView initialExtent world: aWorld
]

{ #category : #'class initialization' }
RealEstateAgent class >> initialize [
	"RealEstateAgent initialize"

	StaggerOffset := 20 @ 40.
	StandardSize := 700@500.
]

{ #category : #accessing }
RealEstateAgent class >> maximumUsableArea [

	| allowedArea |
	allowedArea := Display usableArea.
	allowedArea := allowedArea intersect: self currentWorld visibleClearArea ifNone: [ allowedArea ].
	^allowedArea
]

{ #category : #accessing }
RealEstateAgent class >> maximumUsableAreaInWorld: aWorldOrNil [

	| allowedArea |
	
	allowedArea := 0@0 extent: self standardSize. "fallback default size"
	
	aWorldOrNil ifNotNil: [
		allowedArea := aWorldOrNil displayArea intersect: aWorldOrNil visibleClearArea ifNone: [ allowedArea ]].
	^allowedArea
]

{ #category : #settings }
RealEstateAgent class >> screenTopSetback [
	^ 0
]

{ #category : #settings }
RealEstateAgent class >> scrollBarSetback [
	^ 16-3 "width = 16; inset from border by 3"

]

{ #category : #strategy }
RealEstateAgent class >> staggerFor: aView initialExtent: initialExtent world: aWorld [

	^ self strictlyStaggeredInitialFrameFor: aView
				initialExtent: initialExtent
				world: aWorld
]

{ #category : #strategy }
RealEstateAgent class >> standardFor: aView initialExtent: initialExtent world: aWorld [

	| allOrigins screenRight screenBottom putativeOrigin putativeFrame allowedArea staggerOrigin otherFrames |
	allowedArea := self maximumUsableAreaInWorld: aWorld.
	screenRight := allowedArea right.
	screenBottom := allowedArea bottom.
	otherFrames := (aWorld
				windowsSatisfying: [:w | w isCollapsed not])
				collect: [:w | w bounds].
	allOrigins := otherFrames
				collect: [:f | f origin].
	(self standardPositionsInWorld: aWorld)
		do: [:aPosition | "First see if one of the standard positions is free"
			(allOrigins includes: aPosition)
				ifFalse: ["First see if one of the standard positions is free"
					^ (aPosition extent: initialExtent)
						translatedAndSquishedToBeWithin: allowedArea"First see if one of the standard positions is free"]].
	staggerOrigin := (self standardPositionsInWorld: aWorld) first.
	"Fallback: try offsetting from top left"
	putativeOrigin := staggerOrigin.
	[putativeOrigin := putativeOrigin + StaggerOffset.
	putativeFrame := putativeOrigin extent: initialExtent.
	putativeFrame bottom < screenBottom
		and: [putativeFrame right < screenRight]]
		whileTrue: [(allOrigins includes: putativeOrigin)
				ifFalse: [^ (putativeOrigin extent: initialExtent)
						translatedAndSquishedToBeWithin: allowedArea]].
	^ (self scrollBarSetback @ self screenTopSetback extent: initialExtent)
		translatedAndSquishedToBeWithin: allowedArea
]

{ #category : #accessing }
RealEstateAgent class >> standardPositionsInWorld: aWorldOrNil [
	"Return a list of standard window positions -- this may have one, two, or four of them, depending on the size and shape of the display screen.  "

	| anArea aList  midX midY |
	anArea := self maximumUsableAreaInWorld: aWorldOrNil.
	midX := self scrollBarSetback +   ((anArea width - self scrollBarSetback)  // 2).
	midY := self screenTopSetback + ((anArea height - self screenTopSetback) // 2).
	aList := OrderedCollection with: (self scrollBarSetback @ self screenTopSetback).
	self windowColumnsDesired > 1
		ifTrue: 	[ aList add: (midX @ self screenTopSetback)].
	self windowRowsDesired > 1
		ifTrue: 
			[ aList add: (self scrollBarSetback @ (midY+self screenTopSetback)).
			self windowColumnsDesired > 1 ifTrue:
				[ aList add: (midX @ (midY+self screenTopSetback))]].
	^ aList
]

{ #category : #accessing }
RealEstateAgent class >> standardSize [

	^ StandardSize 
]

{ #category : #accessing }
RealEstateAgent class >> standardSize: extent [

	StandardSize := extent
]

{ #category : #settings }
RealEstateAgent class >> standardWindowExtent [
	"Answer the standard default extent for new windows. "
	| effectiveExtent width strips height grid allowedArea maxLevel |
	effectiveExtent := self maximumUsableArea extent - (self scrollBarSetback @ self screenTopSetback).
	(self usedStrategy = #staggerFor:initialExtent:world:)
		ifTrue: ["NOTE: following copied from
			strictlyStaggeredInitialFrameFor: "
			"Number to be staggered at each corner (less on small
			screens) "
			allowedArea := self maximumUsableArea
						insetBy: (self scrollBarSetback @ self screenTopSetback extent: 0 @ 0).
			"NOTE: following copied from
			strictlyStaggeredInitialFrameFor: "
			"Number to be staggered at each corner (less on small
			screens) "
			maxLevel := allowedArea area > 300000
						ifTrue: [3]
						ifFalse: [2].
			"Amount by which to stagger (less on small screens)"
			grid := allowedArea area > 500000
						ifTrue: [40]
						ifFalse: [20].
			^ allowedArea extent - (grid * (maxLevel + 1 * 2) + (grid // 2)) max: StandardSize].
	width := (strips := self windowColumnsDesired) > 1
				ifTrue: [effectiveExtent x // strips]
				ifFalse: [3 * effectiveExtent x // 4].
	height := (strips := self windowRowsDesired) > 1
				ifTrue: [effectiveExtent y // strips]
				ifFalse: [3 * effectiveExtent y // 4].
	^ width @ height max: StandardSize "RealEstateAgent standardWindowExtent"
]

{ #category : #framing }
RealEstateAgent class >> strictlyStaggeredInitialFrameFor: aStandardSystemView initialExtent: initialExtent world: aWorld [
	"This method implements a staggered window placement policy that I (di) like.
	Basically it provides for up to 4 windows, staggered from each of the 4 corners.
	The windows are staggered so that there will always be a corner visible."

	| allowedArea grid initialFrame otherFrames cornerSel corner delta putativeCorner free maxLevel |

	allowedArea :=(self maximumUsableAreaInWorld: aWorld)
		insetBy: (self scrollBarSetback @ self screenTopSetback extent: 0@0).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel := allowedArea area > 300000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid := allowedArea area > 500000 ifTrue: [40] ifFalse: [20].
	initialFrame := 0@0 extent: ((initialExtent
							"min: (allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))))
							min: 600@400")).
	otherFrames := (aWorld windowsSatisfying: [:w | w isCollapsed not])
					collect: [:w | w bounds].
	0 to: maxLevel do:
		[:level | 
		1 to: 4 do:
			[:ci | cornerSel := #(topLeft topRight bottomRight bottomLeft) at: ci.
			corner := allowedArea perform: cornerSel.
			"The extra grid//2 in delta helps to keep title tabs distinct"
			delta := (maxLevel-level*grid+(grid//2)) @ (level*grid).
			1 to: ci-1 do: [:i | delta := delta rotateBy: #right centerAt: 0@0]. "slow way"
			putativeCorner := corner + delta.
			free := true.
			otherFrames do:
				[:w |
				free := free & ((w perform: cornerSel) ~= putativeCorner)].
			free ifTrue:
				[^ (initialFrame align: (initialFrame perform: cornerSel)
								with: putativeCorner)
						 translatedAndSquishedToBeWithin: allowedArea]]].
	"If all else fails..."
	^ (self scrollBarSetback @ self screenTopSetback extent: initialFrame extent)
		translatedAndSquishedToBeWithin: allowedArea
]

{ #category : #settings }
RealEstateAgent class >> usedStrategy [
	^ UsedStrategy ifNil: [UsedStrategy := #staggerFor:initialExtent:world:]
]

{ #category : #settings }
RealEstateAgent class >> usedStrategy: aSelector [
	UsedStrategy := aSelector
]

{ #category : #settings }
RealEstateAgent class >> windowColumnsDesired [
	"Answer how many separate vertical columns of windows are
	wanted."
	^ (self usedStrategy = #staggerFor:initialExtent:world:)
		ifTrue: [1]
		ifFalse: [self maximumUsableArea width > 640
				ifTrue: [2]
				ifFalse: [1]]
]

{ #category : #settings }
RealEstateAgent class >> windowRowsDesired [
	"Answer how many separate horizontal rows of windows are wanted."
	^ (self usedStrategy = #staggerFor:initialExtent:world:)
		ifTrue: [1]
		ifFalse: [self maximumUsableArea height > 480
				ifTrue: [2]
				ifFalse: [1]] 
]

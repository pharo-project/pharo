*Polymorph-Widgets
justDroppedInto: aMorph event: anEvent
	"Release the mouse focus as well."
	
	isCollapsed
		ifTrue: [
			self position: ((self position max: 0@0) grid: 8@8).
			collapsedFrame := self bounds
		]
		ifFalse: [
			fullFrame := self bounds.
			self isTopWindow ifFalse: [self activate]
		].
	
	anEvent hand releaseMouseFocus.
	
	^super justDroppedInto: aMorph event: anEvent
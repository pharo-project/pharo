geometry
innerBounds
	| inner |
	inner := super innerBounds.
	(submorphs includes: scrollBar) ifTrue:[
		inner := (inner topLeft corner: (inner right - scrollBar width) @ inner bottom)
	].
	self hIsScrollbarShowing
		ifFalse: [ ^ inner]
		ifTrue: [ ^ inner topLeft extent: (inner extent - (0@self scrollBarThickness))].

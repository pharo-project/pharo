private 
extendLayoutDown: delta
	| y heightExtended |
	y := delta.
	heightExtended := 0.
	"Note, the layout will not extend beyond the actual text"
	[ layout height < (y + extent y) ] whileTrue: [ | h |
		" try to extend layout until we cover given point"
		h := layout height.
		layout extendOneLineDown ifFalse: [ ^ heightExtended ].
		heightExtended := heightExtended + (layout height - h)
	].
	^ heightExtended
	
private
doGrow: evt with: growHandle
	"Called while the mouse is down in the grow handle"

	| newExtent extentToUse scale |
	evt hand obtainHalo: self.
	newExtent := (target pointFromWorld: (evt cursorPoint - positionOffset)) - target topLeft.
	evt shiftPressed ifTrue: [
		scale := (newExtent x / (originalExtent x max: 1)) min:
					(newExtent y / (originalExtent y max: 1)).
		newExtent := (originalExtent x * scale) asInteger @ (originalExtent y * scale) asInteger
	].
	(newExtent x < 1 or: [newExtent y < 1 ]) ifTrue: [^ self].
	target renderedMorph extent: (extentToUse := newExtent).
	growHandle position: evt cursorPoint - (growHandle extent // 2).
	self layoutChanged.

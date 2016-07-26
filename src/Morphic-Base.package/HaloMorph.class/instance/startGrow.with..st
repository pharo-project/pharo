private
startGrow: evt with: growHandle
	"Initialize resizing of my target.  Launch a command representing it, to support Undo"

	| botRt |
	self obtainHaloForEvent: evt andRemoveAllHandlesBut: growHandle.
	botRt := target point: target bottomRight in: owner.
	positionOffset := (self world viewBox containsPoint: botRt)
		ifTrue: [evt cursorPoint - botRt]
		ifFalse: [0@0].

	originalExtent := target extent
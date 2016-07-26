private
doScale: evt with: scaleHandle
	"Update the scale of my target if it is scalable."
	| newHandlePos colorToUse |
	evt hand obtainHalo: self.
	newHandlePos := evt cursorPoint - (scaleHandle extent // 2).
	target scaleToMatch: newHandlePos.
	colorToUse := target scale = 1.0
						ifTrue: [Color yellow]
						ifFalse: [Color orange].
	self setColor: colorToUse toHandle: scaleHandle.
	scaleHandle
		submorphsDo: [:m | m color: colorToUse contrastingForegroundColor].
	scaleHandle position: newHandlePos.

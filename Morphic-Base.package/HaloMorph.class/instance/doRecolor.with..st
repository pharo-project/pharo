private
doRecolor: evt with: aHandle
	"The mouse went down in the 'recolor' halo handle.  Allow the user to change the color of the innerTarget"

	evt hand obtainHalo: self.
	(aHandle containsPoint: evt cursorPoint)
		ifFalse:  "only do it if mouse still in handle on mouse up"
			[self delete.
			target addHalo: evt]
		ifTrue: [innerTarget changeColor].
			self showingDirectionHandles ifTrue: [self addHandles]
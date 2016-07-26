private
doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable.  Keep the relevant command object up to date."

	| degrees |
	evt hand obtainHalo: self.
	degrees := (evt cursorPoint - (target pointInWorld: target referencePosition)) degrees.
	degrees := degrees - angleOffset degrees.
	degrees := degrees detentBy: 10.0 atMultiplesOf: 90.0 snap: false.
	degrees = 0.0
		ifTrue: [self setColor: Color lightBlue toHandle: rotHandle]
		ifFalse: [self setColor: Color blue toHandle: rotHandle].
	rotHandle submorphsDo:
		[:m | m color: rotHandle color contrastingForegroundColor].
	self removeAllHandlesBut: rotHandle.
	self showingDirectionHandles ifFalse:
		[self showDirectionHandles: true addHandles: false].
	self addDirectionHandles.
	target rotationDegrees: degrees.
	rotHandle position: evt cursorPoint - (rotHandle extent // 2).

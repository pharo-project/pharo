private
startRot: evt with: rotHandle
	"Initialize rotation of my target if it is rotatable"

	self obtainHaloForEvent: evt andRemoveAllHandlesBut: rotHandle.
	target prepareForRotating.
	growingOrRotating := true.
	angleOffset := evt cursorPoint - (target pointInWorld: target referencePosition).
	angleOffset := Point
			r: angleOffset r
			degrees: angleOffset degrees - target rotationDegrees.


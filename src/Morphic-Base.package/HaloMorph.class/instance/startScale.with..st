private
startScale: evt with: scaleHandle
	"Initialize scaling of my target."

	self obtainHaloForEvent: evt andRemoveAllHandlesBut: scaleHandle.
	target prepareForScaling.
	growingOrRotating := true.
	positionOffset := 0@0.
	originalExtent := target extent

button state
targetPoint
	"Answer the location of the cursor's hotspot, adjusted by the offset
	of the last mouseDown relative to the recipient morph."

	^ position - source targetOffset
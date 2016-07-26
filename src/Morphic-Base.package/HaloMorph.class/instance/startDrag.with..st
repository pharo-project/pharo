dropping/grabbing
startDrag: evt with: dragHandle
	"Drag my target without removing it from its owner."

	self obtainHaloForEvent: evt andRemoveAllHandlesBut: dragHandle.
	positionOffset := dragHandle center - (target point: target position in: owner).
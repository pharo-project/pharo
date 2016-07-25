private
doGrab: evt with: grabHandle
	"Ask hand to grab my target."

	self obtainHaloForEvent: evt andRemoveAllHandlesBut: grabHandle.
	evt hand grabMorph: target.
	self step. "update position if necessary"
	evt hand addMouseListener: self. "Listen for the drop"
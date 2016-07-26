private
obtainHaloForEvent: evt andRemoveAllHandlesBut: aHandle
	"Make sure the event's hand correlates with the receiver, and remove all handles except the given one.  If nil is provided as the handles argument, the result is that all handles are removed.  Note that any pending edits to the name-string in the halo are accepted at this time."

	evt hand obtainHalo: self.
	self removeAllHandlesBut: aHandle
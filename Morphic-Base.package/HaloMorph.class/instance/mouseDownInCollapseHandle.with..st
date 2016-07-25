private
mouseDownInCollapseHandle: evt with: collapseHandle
	"The mouse went down in the collapse handle; collapse the morph"

	self obtainHaloForEvent: evt andRemoveAllHandlesBut: collapseHandle.
	self setDismissColor: evt with: collapseHandle
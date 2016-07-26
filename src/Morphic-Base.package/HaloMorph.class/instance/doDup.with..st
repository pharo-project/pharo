private
doDup: evt with: dupHandle
	"Ask hand to duplicate my target."

	(target isKindOf: SelectionMorph) ifTrue:
		[^ target doDup: evt fromHalo: self handle: dupHandle].

	self obtainHaloForEvent: evt andRemoveAllHandlesBut: dupHandle.
	self setTarget: (target duplicateMorph: evt).
	evt hand grabMorph: target.
	self step. "update position if necessary"
	evt hand addMouseListener: self. "Listen for the drop"
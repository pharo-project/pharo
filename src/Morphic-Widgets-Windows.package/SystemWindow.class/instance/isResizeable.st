controls
isResizeable
	"Answer whether we are not we can be resized."

	^ isResizeable ifNil: [ isResizeable := self defaultIsResizeable ]
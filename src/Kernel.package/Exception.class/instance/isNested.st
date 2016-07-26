testing
isNested
	"Determine whether the current exception handler is within the scope of another handler for the same exception."

	^ handlerContext nextHandlerContext canHandleSignal: self
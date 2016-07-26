printing
longPrintOn: stream
	| ctxt |
	super printOn: stream.
	stream cr.
	ctxt := self suspendedContext.
	[ ctxt isNil ]
		whileFalse: [ 
			stream space.
			ctxt printOn: stream.
			stream cr.
			ctxt := ctxt sender ]
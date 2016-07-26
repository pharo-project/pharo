private-extensions-methodcontext
initializeWith: aPc stackPtr: aStackPtr method: aMethod receiver: aReceiver sender: aSender
	<flExtensionOf: #MethodContext isSource: true>
	^ 'initializeWith: aPc stackPtr: aStackPtr method: aMethod receiver: aReceiver sender: aSender
	pc := aPc.
	stackp := aStackPtr.
	method := aMethod.
	receiver := aReceiver.
	sender := aSender'
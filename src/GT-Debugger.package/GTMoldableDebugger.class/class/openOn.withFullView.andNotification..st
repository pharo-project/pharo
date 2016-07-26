opening api
openOn: aDebugSession withFullView: aBool andNotification: aString
	
	| debugger |
	debugger := (Smalltalk tools 
		debuggerDispatcherFor: aDebugSession interruptedContext
		matching: [:aDebugger | aDebugger availableAutomatically ]) 
			ifNil: [ 
				"Use the current debugger"
				self on: aDebugSession ]
			ifNotNil: [ :aDebugger | 
				"Use a custom extension"
				aDebugger on: (aDebugger spanNewSessionFrom: aDebugSession) ].
	
	aBool | self alwaysOpenFullDebugger
		ifTrue: [ debugger openWithFullView ]
		ifFalse: [ debugger openWithNotification: aString]
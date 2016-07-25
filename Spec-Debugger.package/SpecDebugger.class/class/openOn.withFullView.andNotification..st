opening
openOn: aDebugSession withFullView: aBool andNotification: aString
	
	| debugger |
	debugger := self on: aDebugSession.
	
	aBool | self alwaysOpenFullDebugger
		ifTrue: [ debugger openWithFullView ]
		ifFalse: [ debugger openWithNotification: aString]
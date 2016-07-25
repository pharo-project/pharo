updating
selectionChanged: aContext

	aContext isNotNil ifTrue: [ self announce: (GTGenericStackDebuggerSelectionChanged forDebugger: self) ].

	UIManager default defer: [ 
		(self browser paneNamed: #stack) presentations updateToolbar ].
	
	self class enableStackColoring ifFalse: [ ^ self ].  

	UIManager default defer: [
		(self browser paneNamed: #stack) presentations presentations ifNotEmpty: [ :p | 
			self session interruptedProcess ifNotNil: [ p first update ] ] ]
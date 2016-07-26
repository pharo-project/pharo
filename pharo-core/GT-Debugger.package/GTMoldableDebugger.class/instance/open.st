scripting opening
open		
	| window |
	window := super open.
	
	(Smalltalk hasClassNamed: #GTDebuggerEventCollector) ifTrue: [ 
		#GTDebuggerEventCollector asClass new collectDataFrom: self ].
		
	^ window.
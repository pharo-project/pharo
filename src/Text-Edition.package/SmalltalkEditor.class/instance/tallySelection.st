do-its
tallySelection
	"Treat the current selection as an expression; evaluate and tally it."
	| receiver context v  compiledMethod profiler |
	(model respondsTo: #doItReceiver) 
		ifTrue: 
			[receiver := model doItReceiver.
			context := model doItContext]
		ifFalse:
			[receiver := context := nil].
	self lineSelectAndEmptyCheck: [ ^ self ].
	compiledMethod := self compileSelectionFor: receiver in: context.
	compiledMethod ifNil: [^ self].
	profiler := TimeProfiler spyOn: [ v:= compiledMethod valueWithReceiver: receiver arguments: #()].
	profiler showResult: v.
	

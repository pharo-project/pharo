do-its
tally: aStream
	"Treat the current selection as an expression; evaluate and tally it."
	| receiver context v compiledMethod profiler |
	(self model respondsTo: #doItReceiver) 
		ifTrue: 
			[receiver := self model doItReceiver.
			context := self model doItContext]
		ifFalse:
			[receiver := context := nil].
	compiledMethod := self compile: aStream for: receiver in: context.
	compiledMethod ifNil: [^ self].
	profiler := TimeProfiler spyOn: [ v:= compiledMethod valueWithReceiver: receiver arguments: #()].
	profiler showResult: v.
	

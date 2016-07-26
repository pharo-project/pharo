source mapping
abstractSourceMap
	"Answer with a Dictionary of abstractPC <Integer> to sourceRange <Interval>."
	| theMethodToScan rawSourceRanges concreteSourceRanges abstractPC scanner client |
	abstractSourceRanges ifNotNil:
		[^abstractSourceRanges].
	"If the methodNode hasn't had a method generated it doesn't have pcs set in its
	 nodes so we must generate a new method and might as well use it for scanning."
	methodNode rawSourceRangesAndMethodDo:
		[:ranges :method|
		 rawSourceRanges := ranges.
		 theMethodToScan := method].
	concreteSourceRanges := Dictionary new.
	rawSourceRanges keysAndValuesDo:
		[:node :range|
		node pc ~= 0 ifTrue:
			[concreteSourceRanges at: node pc put: range]].
	abstractPC := 1.
	abstractSourceRanges := Dictionary new.
	scanner := InstructionStream on: theMethodToScan.
	client := InstructionClient new.
	[(concreteSourceRanges includesKey: scanner pc) ifTrue:
		[abstractSourceRanges at: abstractPC put: (concreteSourceRanges at: scanner pc)].
	 abstractPC := abstractPC + 1.
	 scanner interpretNextInstructionFor: client.
	 scanner atEnd] whileFalse.
	^abstractSourceRanges
visual properties
fillStyleToUse
	"Answer the fill style to used based on the current state."

	|map|
	map := self active
		ifTrue: [self stateMap at: #active ifAbsent: [self stateMap at: #passive ifAbsent: [Dictionary new]]]
		ifFalse: [self stateMap at: #passive ifAbsent: [self stateMap at: #active ifAbsent: [Dictionary new]]].
	map := self enabled
		ifTrue: [map at: #enabled ifAbsent: [map at: #disabled ifAbsent: [Dictionary new]]]
		ifFalse: [map at: #disabled ifAbsent: [map at: #enabled ifAbsent: [Dictionary new]]].
	map := self over
		ifTrue: [map at: #over ifAbsent: [map at: #notOver ifAbsent: [Dictionary new]]]
		ifFalse: [map at: #notOver ifAbsent: [map at: #over ifAbsent: [Dictionary new]]].
	^map at: (self down ifTrue: [#down] ifFalse: [#up]) ifAbsent: [
		map at: (self down ifTrue: [#up] ifFalse: [#down]) ifAbsent: [Color transparent]]
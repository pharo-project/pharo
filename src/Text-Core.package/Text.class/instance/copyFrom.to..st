copying
copyFrom: start to: stop 
	"Answer a copied subrange of the receiver."

	| realStart realStop |
	stop > self size
		ifTrue: [realStop := self size]		"handle selection at end of string"
		ifFalse: [realStop := stop].
	start < 1
		ifTrue: [realStart := 1]			"handle selection before start of string"
		ifFalse: [realStart := start].
	^self class 
		string: (string copyFrom: realStart to: realStop)
		runs: (runs copyFrom: realStart to: realStop)
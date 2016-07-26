tests - indexable access
testAllButLastElements
	"self debug: #testAllButFirst"
	| abf col |
	col := self accessCollection.
	abf := col allButLast: 2.
	1 
		to: abf size
		do: [ :i | self assert: (abf at: i) = (col at: i) ].
	self assert: abf size + 2 = col size
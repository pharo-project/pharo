tests - subcollections access
testAllButFirstNElements
	"self debug: #testAllButFirst"
	| abf col |
	col := self moreThan3Elements.
	abf := col allButFirst: 2.
	1 
		to: abf size
		do: [ :i | self assert: (abf at: i) = (col at: i + 2) ].
	self assert: abf size + 2 = col size
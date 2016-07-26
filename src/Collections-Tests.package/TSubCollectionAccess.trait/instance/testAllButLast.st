tests - subcollections access
testAllButLast
	"self debug: #testAllButLast"
	| abf col |
	col := self moreThan3Elements.
	abf := col allButLast.
	self deny: abf last = col last.
	self assert: abf size + 1 = col size
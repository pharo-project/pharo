tests - subcollections access
testAllButFirst
	"self debug: #testAllButFirst"
	| abf col |
	col := self moreThan3Elements.
	abf := col allButFirst.
	self deny: abf first = col first.
	self assert: abf size + 1 = col size
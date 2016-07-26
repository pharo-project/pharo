tests
testAddAfterLast2
	"LinkedListTest new testAddAfterLast2"

	| l first second third fourth |
	l := LinkedList new.
	first := self class new n: 1.
	second := self class new n: 2.
	third := self class new n: 3.
	fourth :=self class new n: 4.
	l addLast: first.
	l addLast: second.
	self assert: (l collect:[:e | e n]) asArray  = #(1 2). 
	l add: third after: second.
	self assert: (l collect:[:e | e n]) asArray  = #(1 2 3).
	l addLast: fourth.
	self assert: (l collect:[:e | e n]) asArray  = #(1 2 3 4).
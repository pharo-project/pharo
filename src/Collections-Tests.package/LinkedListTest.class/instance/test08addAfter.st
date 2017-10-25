tests
test08addAfter
	| l first |
	l := LinkedList new.
	first := self class new n: 1.
	
	l add: first.
	l add: (self class new n: 3).

	self assert: (l collect:[:e | e n]) asArray  = #(1 3).
	
	l add: (self class new n: 2) after: first.

	self assert: (l collect:[:e | e n]) asArray  = #(1 2 3).
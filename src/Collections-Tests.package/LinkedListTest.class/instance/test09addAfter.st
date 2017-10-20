tests
test09addAfter
	| l last |
	l := LinkedList new.
	last := self class new n: 2.
	l add: (self class new n: 1).
	l add: last.
	
	self assert: (l collect:[:e | e n]) asArray  = #(1 2).
	 
	l add: (self class new n: 3) after: last.

	self assert: (l collect:[:e | e n]) asArray  = #(1 2 3).
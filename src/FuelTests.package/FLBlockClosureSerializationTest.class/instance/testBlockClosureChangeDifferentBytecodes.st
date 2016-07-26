tests-change
testBlockClosureChangeDifferentBytecodes
	"Raise an error when materializing a closure whose method has changed bytecodes."

	| aClass aClosure |
	aClass := self newClass
		duringTestCompileSilently: 'methodWithClosure  ^ [ 42 ]';
		yourself.
	aClosure := aClass new perform: #methodWithClosure.
	self serialize: aClosure.
	aClass duringTestCompileSilently: 'methodWithClosure  ^ 42'.
	self should: [ self materialized ] raise: FLMethodChanged
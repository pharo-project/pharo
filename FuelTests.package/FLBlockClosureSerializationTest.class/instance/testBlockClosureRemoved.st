tests-change
testBlockClosureRemoved
	"Raise an error when materializing a closure whose method was removed."

	| aClass aClosure |
	aClass := self newClass
		duringTestCompileSilently: 'methodWithClosure  ^ [ 42 ]';
		yourself.
	aClosure := aClass new perform: #methodWithClosure.
	self serialize: aClosure.
	aClass removeSelectorSilently: #methodWithClosure.
	self should: [ self materialized ] raise: FLMethodNotFound
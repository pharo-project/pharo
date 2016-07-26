tests-change
testBlockClosureChangeSameBytecodes
	"Tolerate materializing a closure whose method has changed but not the bytecodes."

	| aClass aClosure materializedClosure |
	aClass := self newClass
		duringTestCompileSilently: 'methodWithClosure  ^ [ 41 ]';
		yourself.
	aClosure := aClass new perform: #methodWithClosure.
	self serialize: aClosure.
	aClass duringTestCompileSilently: 'methodWithClosure  ^ [ 42 ]'.
	self deny: aClosure method isInstalled.
	materializedClosure := self materialized.
	self assert: materializedClosure value = 42
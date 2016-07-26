tests
testMethodRecompilationShouldNotLog

	| headBeforeRecompiling |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.

	headBeforeRecompiling := monitor head.

	aClass compile: 'fortyTwo ^42'.
	
	self assert: monitor head == headBeforeRecompiling
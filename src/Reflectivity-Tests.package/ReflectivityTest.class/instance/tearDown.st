running
tearDown
	link ifNotNil: [link uninstall].
	link2 ifNotNil: [link2 uninstall].
	tag := nil.
	count := 0.
	(ReflectivityExamples>>#exampleWithArg:)  recompile
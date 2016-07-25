running
tearDown
	link ifNotNil: [link uninstall].
	ReflectivityExamples recompile: selector.
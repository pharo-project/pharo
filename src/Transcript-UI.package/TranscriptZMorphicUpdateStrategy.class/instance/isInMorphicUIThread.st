testing
isInMorphicUIThread

	^(UIManager default respondsTo: #uiProcess) and: [
		Processor activeProcess == UIManager default uiProcess
	]
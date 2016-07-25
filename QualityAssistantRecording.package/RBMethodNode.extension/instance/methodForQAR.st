*qualityAssistantRecording
methodForQAR

	self compilationContext ifNotNil: [ :contx |
		contx getClass ifNotNil: [ :class |
			class
				compiledMethodAt: self selector
				ifPresent: [ :meth | ^ meth ]
				ifAbsent: [ ] ] ].
	
	^ self compiledMethod
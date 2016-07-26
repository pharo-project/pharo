events
populateInputOfStep: aGLMWiizardStep

	self stepToUse previousStep isEmpty
		ifTrue: [
			self stepToUse input: (self pane port: #entity) value.
			]
		ifFalse: [
			(self stepToUse previousStep size = 1)
				ifTrue: [
					self stepToUse input: ((self stepsAndPanes at: self stepToUse previousStep anyOne) last port: #selection) value.
					]
				ifFalse: [
					|tmpInputs|
					tmpInputs := self stepToUse previousStep collect: [:aSymbol |
						((self stepsAndPanes at: aSymbol) last port: #selection) value
						].
					self stepToUse input: tmpInputs
					].
			].
	
			
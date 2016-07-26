private
cachedExamplesDo: aBlock
	examples do: [ :selectorsAndExamples |
		selectorsAndExamples do: aBlock ]
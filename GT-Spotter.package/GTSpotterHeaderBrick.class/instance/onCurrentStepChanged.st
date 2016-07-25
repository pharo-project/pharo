event handling
onCurrentStepChanged

	self searchField rubric updateTextWith: self spotterModel currentStep context text.
	self searchField rubric selectFrom: self searchField rubric text size + 1 to: self searchField rubric text size
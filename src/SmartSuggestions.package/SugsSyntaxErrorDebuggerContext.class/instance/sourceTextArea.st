refactoring
sourceTextArea
	^ model dependents first findDeepSubmorphThat: [ :m | m isKindOf: RubScrolledTextMorph ] ifAbsent: [ self error: 'should not happen' ]
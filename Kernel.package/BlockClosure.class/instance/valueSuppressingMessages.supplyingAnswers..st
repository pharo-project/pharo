evaluating
valueSuppressingMessages: aListOfStrings supplyingAnswers: aListOfPairs

	^ self valueSupplyingAnswers: aListOfPairs, (aListOfStrings collect: [:each | {each. true}])
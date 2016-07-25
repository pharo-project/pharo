execution
execute
	| selectedValidCodeInterval |
	selectedValidCodeInterval := self privateCalculateInterval.
	(NautilusRefactoring model: context) extractBetween: selectedValidCodeInterval from: context selectedMethod
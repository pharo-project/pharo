tests
testTempsReadBeforeWritten
	self ruleFor: self currentSelector
		plusSelectors: #(#inlineTemporary #noMoveDefinition #tempsReadBeforeWritten #equalNotUsed #fileBlocks #referencesConditionFor:)
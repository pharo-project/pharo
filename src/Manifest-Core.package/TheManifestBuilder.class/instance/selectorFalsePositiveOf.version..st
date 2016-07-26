private
selectorFalsePositiveOf: ruleId version: versionId

	^ (self class falsePositiveBeginningTag, ruleId,
		self class falsePositiveMiddleTag, versionId asString,
		self class falsePositiveEndTag) 
			asSymbol.
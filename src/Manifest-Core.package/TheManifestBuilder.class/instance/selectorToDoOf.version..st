private
selectorToDoOf: ruleId version: versionId

	^ (self class toDoBeginningTag, ruleId ,
		self class toDoMiddleTag, versionId asString,
		self class toDoEndTag) asSymbol.
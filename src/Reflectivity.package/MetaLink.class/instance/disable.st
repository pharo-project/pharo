reflecive api
disable
	"turn off link reflectively while keeping the condition"
	self hasReifiedCondition
		ifFalse: [self condition: (RFCondition for: condition)].
	self condition disable.
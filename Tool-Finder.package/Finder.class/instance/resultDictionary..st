accessing
resultDictionary: aDictionary
	"When the result dictionary is update, I send an event through the whole universe"

	resultDictionary := aDictionary.
	self triggerEvent: #updateResultDictionary.
	self selectedMethod: nil.
	self selectedClass: nil.
	self triggerEvent: #updateSourceCode.
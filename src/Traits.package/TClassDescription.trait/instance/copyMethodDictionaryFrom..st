copying
copyMethodDictionaryFrom: donorClass
	"Copy the method dictionary of the donor class over to the receiver"

	self methodDict: donorClass copyOfMethodDictionary.
	self organization: donorClass organization deepCopy.
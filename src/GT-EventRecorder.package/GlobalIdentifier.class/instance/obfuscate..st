actions
obfuscate: anObject
	"If you have an information that you want to obfuscate before sending,
	this is likely a good solution for you."
	^ anObject hash bitXor: self secretUUID hash
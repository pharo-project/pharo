accessing
secretUUID
	"This value can be used for obfuscating an information before sending it.
	See #obfuscate: method.
	Before calling #secretUUID method, you should call #ensure."
	^ persistedInformation at: #secretUUID
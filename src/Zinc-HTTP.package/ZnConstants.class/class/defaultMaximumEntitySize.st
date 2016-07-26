resource limits
defaultMaximumEntitySize
	"Return the default maximum entity size to accept from a stream.
	Used by ZnEntity and subclasses. Can be set using #defaultMaximumEntitySize:
	Default value is 16Mb. This affects both client and server code.
	This helps to protect us from malicious content."
	
	^ DefaultMaximumEntitySize ifNil: [ DefaultMaximumEntitySize := 16 * 1024 * 1024 ] 
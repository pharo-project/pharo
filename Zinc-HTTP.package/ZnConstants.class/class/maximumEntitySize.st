resource limits
maximumEntitySize
	"Return the maximum entity size to accept from a stream.
	Used by ZnEntity and subclasses. Can be set using #maximumEntitySize:
	This affects both client and server code.
	This helps to protect us from malicious content."
	
	^ ZnMaximumEntitySize value 
		ifNil: [ self defaultMaximumEntitySize ]
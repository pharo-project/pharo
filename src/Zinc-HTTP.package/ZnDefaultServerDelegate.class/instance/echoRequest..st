responses
echoRequest: request	
	"Echo request by generating a plain text response useful for debugging purposes"
	
	| delay info entity |
	delay := Integer readFrom: (request uri queryAt: 'delay' ifAbsent: [ '' ]) ifFail: [ 0 ].
	delay > 0 ifTrue: [ (Delay forSeconds: delay) wait ].  
	info := self generateEchoRequestString: request.
	entity := ZnEntity textCRLF: info.
	^ ZnResponse ok: entity
responses
sessionRequest: request	
	"An HTML response that uses a session"
	
	| session hitCount |
	session := request session.
	hitCount := session attributeAt: #hitCount ifAbsent: [ 0 ].
	hitCount := hitCount + 1.
	session attributeAt: #hitCount put: hitCount. 
	^ ZnResponse ok: (ZnEntity html: (self generateSessionRequest: session))
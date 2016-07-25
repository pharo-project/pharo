system integration
systemMethodAddedActionFrom: ann
	| method |
	
	"If the method origin is not the one of the event, we do not care about that method"
	
	method := ann method.
	method origin = ann methodClass
		ifFalse: [ ^ self ].
		
	self addMethod: method
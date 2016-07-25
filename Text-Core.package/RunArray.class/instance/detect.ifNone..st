enumerating
detect: aBlock ifNone: exceptionBlock 
	"Only evaluate once per run"

	^values detect: aBlock ifNone: exceptionBlock 
scanning
selectorToSendOrSelf
	"If this instruction is a send, answer the selector, otherwise answer self."
 
	^ self method encoderClass selectorToSendOrItselfFor: self in: self method at: pc 
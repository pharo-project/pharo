accessing
doItReceiver: aOneArgBlock
	"The block will be passed the current entity and is expected to return an object .This object will then be returned upon evaluation of 'self' within the presentation."
	doItReceiver := aOneArgBlock
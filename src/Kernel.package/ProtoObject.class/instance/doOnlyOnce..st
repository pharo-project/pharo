debugging
doOnlyOnce: aBlock
	"If the 'one-shot' mechanism is armed, evaluate aBlock once and disarm the one-shot mechanism.  To rearm the mechanism, evaluate  'self rearmOneShot' manually."

	(self class environment at: #OneShotArmed ifAbsent: [ true ])
		ifTrue: [ 
			self class environment at: #OneShotArmed put: false.
			aBlock value ]
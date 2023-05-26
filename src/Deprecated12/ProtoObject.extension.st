Extension { #name : #ProtoObject }

{ #category : #'*Deprecated12' }
ProtoObject >> doOnlyOnce: aBlock [
	"If the 'one-shot' mechanism is armed, evaluate aBlock once and disarm the one-shot mechanism.  To rearm the mechanism, evaluate  'self rearmOneShot' manually."

	Deprecation new
		context: thisContext sender;
		explanation: 'Deprecated in Pharo 12 without replacement - see issue #13310';
		signal.

	(self class environment at: #OneShotArmed ifAbsent: [ true ])
		ifTrue: [
			self class environment at: #OneShotArmed put: false.
			aBlock value ]
]

{ #category : #'*Deprecated12' }
ProtoObject >> rearmOneShot [
	"Call this manually to arm the one-shot mechanism; use the mechanism in code by calling
		self doOnlyOnce: <a block>"

	Deprecation new
		context: thisContext sender;
		explanation: 'Deprecated in Pharo 12 without replacement - see issue #13310';
		signal.

	self class environment at: #OneShotArmed put: true

	"self rearmOneShot"
]

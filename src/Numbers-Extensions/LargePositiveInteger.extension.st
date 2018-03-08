Extension { #name : #LargePositiveInteger }

{ #category : #'*Numbers-Extensions' }
LargePositiveInteger >> ln [
	"This function is defined because super ln might overflow."
	| res h |
	res := super ln.
	res isFinite ifTrue: [^res].
	h := self highBit.
	^2 ln * h + (self / (1 << h)) asFloat ln
]

{ #category : #'*Numbers-Extensions' }
LargePositiveInteger >> log [
	"This function is defined because super log might overflow."
	| res h |
	res := super log.
	res isFinite ifTrue: [^res].
	h := self highBit.
	^2 log * h + (self / (1 << h)) asFloat log
]

{ #category : #'*Numbers-Extensions' }
LargePositiveInteger >> sqrt [
	"If we know for sure no exact solution exists, then just answer the cheap float approximation without wasting time."
	| selfAsFloat |
	self mightBeASquare
		ifFalse:
			[selfAsFloat := self asFloat.
			selfAsFloat isFinite ifTrue: [^self asFloat sqrt ]].

	"If some exact solution might exist, or self asFloat isInfinite, call potentially expensive super"
	^super sqrt
]

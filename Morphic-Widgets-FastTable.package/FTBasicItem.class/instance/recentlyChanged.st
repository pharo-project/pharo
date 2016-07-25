accessing
recentlyChanged
	"Since I must be true only if I collapsed recently, I change to false if someone get me."

	^ recentlyChanged
		ifFalse: [ false ]
		ifTrue: [ recentlyChanged := false.
			true ]
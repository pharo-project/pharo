system compatibility
overriddenMethodsDo: aBlock
	"Enumerates the methods the receiver contains which have been overridden by other packages"
	^ self allOverriddenMethodsDo: [:ea |
		(self isOverrideOfYourMethod: ea)
			ifTrue: [aBlock value: ea]]
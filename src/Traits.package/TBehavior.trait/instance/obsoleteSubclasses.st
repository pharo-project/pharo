obsolete subclasses
obsoleteSubclasses
	"Return all the weakly remembered obsolete subclasses of the receiver"
	| obs |
	obs := self basicObsoleteSubclasses at: self ifAbsent: [^ #()].
	^ obs copyWithout: nil
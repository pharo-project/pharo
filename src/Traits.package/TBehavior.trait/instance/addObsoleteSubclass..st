obsolete subclasses
addObsoleteSubclass: aClass
	"Weakly remember that aClass was a subclass of the receiver and is now obsolete"
	| obs |

	obs := self basicObsoleteSubclasses at: self ifAbsent:[WeakArray new].
	(obs includes: aClass) ifTrue:[^self].
	obs := obs copyWithout: nil.
	obs := obs copyWith: aClass.
	self basicObsoleteSubclasses at: self put: obs.
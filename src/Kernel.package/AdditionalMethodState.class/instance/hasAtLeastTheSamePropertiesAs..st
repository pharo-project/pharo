testing
hasAtLeastTheSamePropertiesAs: aMethodProperties
	"Answer if the recever has at least the same properties as the argument.
	 N.B. The receiver may have additional properties and still answer true."
	aMethodProperties keysAndValuesDo:
		[:k :v|
		(v isKindOf: Pragma)
			"ifTrue: [Pragmas have already been checked]"
			ifFalse: [
				(self includes: k->v) ifFalse: [^false]]].
	^true
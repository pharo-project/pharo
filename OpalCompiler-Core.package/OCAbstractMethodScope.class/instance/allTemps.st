temp vars
allTemps
	"return all temps defined, even the ones in the outer scope that are not used in the current.
	 This includes the arguments
	 We do not need to care about shadowed temps as temp shadowing is not allowed."
	| temps |
	temps := OrderedCollection new.
	self outerScope allTemps do: [ :var |
	 	(self localTempNames includes: var name) ifFalse: [
			temps add: var.]
		].
	temps addAll: self localTemps.
	^temps asArray.
	
accessing
optionAt: key put: value
	"Set my option/setting identified by key to be value.
	This is a generic interface, see my options protocol for specific usages."
	
	options ifNil: [ options := Dictionary new ].
	options at: key put: value
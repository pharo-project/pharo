accessing
optionAt: key ifAbsentPut: block
	"Return my option/settings stored under key.
	If I have no such option/setting, store the result of evaluating block as new value and return it.
	This is a generic interface, see my options protocol for specific usages."

	options ifNil: [ options := Dictionary new ].
	^ options at: key ifAbsentPut: block
accessing
optionAt: key ifAbsent: block
	"Return my option/settings stored under key.
	Execute block if I have no such option/setting.
	This is a generic interface, see my options protocol for specific usages."

	options ifNil: [ ^ block value ].
	^ options at: key ifAbsent: block
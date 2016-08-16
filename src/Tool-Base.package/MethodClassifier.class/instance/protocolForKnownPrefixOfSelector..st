private
protocolForKnownPrefixOfSelector: aSelector
	"Returns the protocol for the first prefix that equals aSelector or that
	aSelector starts with if the prefix is followed by a non-lowercase char
	(like $: or $A) or nil if there isn't one."

	| selectorString |
	selectorString := aSelector asString.
	self class prefixMapping
		keysAndValuesDo: [ :prefix :protocol | 
			(selectorString = prefix
				or: [ (selectorString beginsWith: prefix)
						and: [ (selectorString at: prefix size + 1) isLowercase not ] ])
				ifTrue: [ ^ protocol ] ].
	^ nil
double dispatch
syntaxRegex: regexNode
	"All prefixes of the regex's branches should be combined.
	Therefore, just recurse."

	regexNode branch dispatchTo: self.
	regexNode regex notNil
		ifTrue: [regexNode regex dispatchTo: self]
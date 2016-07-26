parsing
removePhrase
	"skip most characters to the left of this"

	[
		tokens isEmpty not and: [
			#(Atom QuotedString $. $@) includes: (tokens last type) ]
	] whileTrue: [ tokens removeLast ].

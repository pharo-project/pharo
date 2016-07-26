error correction
substituteSelector: selectorParts wordIntervals: spots
	"Substitute the correctSelector into the (presuamed interactive) receiver."
	| offset |
	offset := 0.
	selectorParts with: spots do:
		[ :word :interval |
		offset := self substituteWord: word wordInterval: interval offset: offset ]

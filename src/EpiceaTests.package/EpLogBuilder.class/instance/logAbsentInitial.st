building
logAbsentInitial
	"Put an absent prior as head"

	| otherLog |
	otherLog := self class new
		logInitial;
		log.
	log headReference: otherLog headReference.
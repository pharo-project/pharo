Extension { #name : #DateAndTime }

{ #category : #'*Network-Mail' }
DateAndTime >> asEmailString [
	"Format per RFC5322  e.g.  'Thu, 18 Feb 1999 20:38:51 -0500'"

	| timeString |
	timeString := (self asTime print24 first: 8).
	^ '{1}, {2} {3} {4} {5} {6}' format: { 
			self dayOfWeekAbbreviation.
			self dayOfMonth asString.
			self monthAbbreviation.
			self year asString.
			timeString.
			self offset asEmailTimeOffsetString }.
]

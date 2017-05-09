pharo - updates
update60483
	"self new update60483"
	self withUpdateLog: '20023 #isDeprecated does not cover all deprecation messsages
	https://pharo.fogbugz.com/f/cases/20023

19920 FTTableMorph implicit filter isnt working properly with the escape keystroke
	https://pharo.fogbugz.com/f/cases/19920'.
	self loadTogether: self script60483 merge: false.
	self flushCaches.

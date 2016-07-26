tests tally
setUp
	tally := QSystemTally new.
	tally class: self class method: self class >> #testPrintingTally	
	"tally class: Object method: Object >> #printString."
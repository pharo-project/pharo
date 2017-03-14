pharo - updates
update60441
	"self new update60441"
	self withUpdateLog: '19826 Off by one error in FTTableContainerMorph>>#calculateStartIndexWhenShowing:
	https://pharo.fogbugz.com/f/cases/19826

19828 Class side methods + hierarchy selection eats the selected class. 
	https://pharo.fogbugz.com/f/cases/19828'.
	self loadTogether: self script60441 merge: false.
	self flushCaches.

pharo - updates
update60474
	"self new update60474"
	self withUpdateLog: '19982 Better method comments
	https://pharo.fogbugz.com/f/cases/19982

19925 Document that SmallInteger>>printString breaks the #printOn: rule 
	https://pharo.fogbugz.com/f/cases/19925

19999 Improve SHTextStylerST80 class side protocols
	https://pharo.fogbugz.com/f/cases/19999

19990 UFFI not reset method cache at image startup
	https://pharo.fogbugz.com/f/cases/19990'.
	self loadTogether: self script60474 merge: false.
	self flushCaches.

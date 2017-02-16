pharo - updates
update60396
	"self new update60396"
	self withUpdateLog: '19716 The method Pragma>>#selector called from MetacelloToolBox
	https://pharo.fogbugz.com/f/cases/19716

19713 improve #testObjectFormatInstSize
	https://pharo.fogbugz.com/f/cases/19713'.
	self loadTogether: self script60396 merge: false.
	self flushCaches.

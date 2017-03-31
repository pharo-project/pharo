pharo - updates
update60456
	"self new update60456"
	self withUpdateLog: '19888 FastTable should not reset highlighting rows when selection is changed
	https://pharo.fogbugz.com/f/cases/19888

19887 cairo surface does not need to set lock/show surface callbacks 
	https://pharo.fogbugz.com/f/cases/19887'.
	self loadTogether: self script60456 merge: false.
	self flushCaches.

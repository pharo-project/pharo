pharo - updates
update60464
	"self new update60464"
	self withUpdateLog: '19884 Rubric should not use SmallInteger maxVal
	https://pharo.fogbugz.com/f/cases/19884'.
	self loadTogether: self script60464 merge: false.
	self flushCaches.

pharo - updates
update60305
	"self new update60305"
	self withUpdateLog: '19295 originAtCenter looks like deadCode
	https://pharo.fogbugz.com/f/cases/19295

19372 MethodFinderTest is dependent>>#testSearchForAccessAtOne is dependent on result order
	https://pharo.fogbugz.com/f/cases/19372

17771 Empty stderr/stdout files should be deleted on image shutdown
	https://pharo.fogbugz.com/f/cases/17771'.
	self loadTogether: self script60305 merge: false.
	self flushCaches.

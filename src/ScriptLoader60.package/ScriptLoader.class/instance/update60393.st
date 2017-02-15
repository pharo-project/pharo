pharo - updates
update60393
	"self new update60393"
	self withUpdateLog: '19710 Monticello should ignore the class definition of Context
	https://pharo.fogbugz.com/f/cases/19710

19711 RPackage should not strongly depend on Monticello
	https://pharo.fogbugz.com/f/cases/19711

19709 add example ComputedSlot
	https://pharo.fogbugz.com/f/cases/19709'.
	self loadTogether: self script60393 merge: false.
	self flushCaches.

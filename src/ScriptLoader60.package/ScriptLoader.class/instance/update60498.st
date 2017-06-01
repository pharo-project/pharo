pharo - updates
update60498
	"self new update60498"
	self withUpdateLog: '20095 SystemProgressMorph should call doOneCycleNow on refresh only when active process is UIProcess
	https://pharo.fogbugz.com/f/cases/20095

20099 Cairo fonts crashing on 64bits
	https://pharo.fogbugz.com/f/cases/20099'.
	self loadTogether: self script60498 merge: false.
	self flushCaches.

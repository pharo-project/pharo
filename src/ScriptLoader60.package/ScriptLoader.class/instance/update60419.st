pharo - updates
update60419
	"self new update60419"
	self withUpdateLog: '19781 callback with return of void* leads to an invalid return
	https://pharo.fogbugz.com/f/cases/19781

19783 prepare SDL2 for 64bits
	https://pharo.fogbugz.com/f/cases/19783

19782 prepare Athens for 64bits
	https://pharo.fogbugz.com/f/cases/19782'.
	self loadTogether: self script60419 merge: false.
	self loadConfiguration: 'UnifiedFFI' version: '0.26.1'.self flushCaches.

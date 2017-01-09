pharo - updates
update60340
	"self new update60340"
	self withUpdateLog: '19529 Mouseover an empty submenu causes UI lockdown
	https://pharo.fogbugz.com/f/cases/19529

19526 readString should check NULL condition
	https://pharo.fogbugz.com/f/cases/19526

19520 Epicea: Tests lacking files cleanup
	https://pharo.fogbugz.com/f/cases/19520'.
	self loadTogether: self script60340 merge: false.
	self loadConfiguration: 'UnifiedFFI' version: '0.25.1'.
self loadConfiguration: 'Epicea' version: '8.0.5'.self flushCaches.

pharo - updates
update60503
	"self new update60503"
	self withUpdateLog: '20167 Regression with PNGReaderWriter in P6
	https://pharo.fogbugz.com/f/cases/20167

20198 inner structure access does not work on multiple architecture
	https://pharo.fogbugz.com/f/cases/20198'.
	self loadTogether: self script60503 merge: false.
	self loadConfiguration: 'UnifiedFFI' version: '0.26.8'.self flushCaches.

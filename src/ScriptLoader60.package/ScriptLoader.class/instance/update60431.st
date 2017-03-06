pharo - updates
update60431
	"self new update60431"
	self withUpdateLog: '19805 LibC>>loadLibrary: works just in macOS (no win or linux)
	https://pharo.fogbugz.com/f/cases/19805

18472 GTSpotter settings are unusable
	https://pharo.fogbugz.com/f/cases/18472'.
	self loadTogether: self script60431 merge: false.
	self loadConfiguration: 'UnifiedFFI' version: '0.26.4'.self flushCaches.

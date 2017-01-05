pharo - updates
update60336
	"self new update60336"
	self withUpdateLog: '19493 Load new FastTable config
	https://pharo.fogbugz.com/f/cases/19493

19488 STONWriterTests>>#testDictionaryWithComplexKeys is order dependent
	https://pharo.fogbugz.com/f/cases/19488

19495 TabMorph should fork morph building in lesser priority than active process
	https://pharo.fogbugz.com/f/cases/19495'.
	self loadTogether: self script60336 merge: false.
	self loadConfiguration: 'FastTable' version: '0.10.7'.self flushCaches.

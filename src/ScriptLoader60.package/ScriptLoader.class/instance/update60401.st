pharo - updates
update60401
	"self new update60401"
	self withUpdateLog: '19488 STONWriterTests>>#testDictionaryWithComplexKeys is order dependent
	https://pharo.fogbugz.com/f/cases/19488

18584 DNU on showing menu in PointerExplorer
	https://pharo.fogbugz.com/f/cases/18584

18459 FileList calls unimplemented method allRegisteredServices
	https://pharo.fogbugz.com/f/cases/18459

18724 DNU EyeTreeInspector workspace pane context menu
	https://pharo.fogbugz.com/f/cases/18724'.
	self loadTogether: self script60401 merge: false.
	self flushCaches.

pharo - updates
update60324
	"self new update60324"
	self withUpdateLog: '19445 Pattern code ArrayNode matching failure
	https://pharo.fogbugz.com/f/cases/19445

18935 Rubric warning are not DarkTheme friendly
	https://pharo.fogbugz.com/f/cases/18935

19455 Testcase for reify receiver for before links
	https://pharo.fogbugz.com/f/cases/19455'.
	self loadTogether: self script60324 merge: false.
	self flushCaches.

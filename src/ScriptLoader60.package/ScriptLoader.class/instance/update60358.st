pharo - updates
update60358
	"self new update60358"
	self withUpdateLog: '19315 Freetype2 primitive failed [error 2][unknown file format]
	https://pharo.fogbugz.com/f/cases/19315

19610 NEC has unacceptable performance when selectors == false
	https://pharo.fogbugz.com/f/cases/19610

18232 Breakpoints cannot be toggled in Nautilus method pane
	https://pharo.fogbugz.com/f/cases/18232'.
	self loadTogether: self script60358 merge: false.
	self flushCaches.

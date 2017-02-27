pharo - updates
update60412
	"self new update60412"
	self withUpdateLog: '19756 Usage of #name in NautilusRefactoring
	https://pharo.fogbugz.com/f/cases/19756

19752 Object>>#name deprecation in SpecDebugger>>#session: 
	https://pharo.fogbugz.com/f/cases/19752

19758 prevent surface GC when using it to draw a form
	https://pharo.fogbugz.com/f/cases/19758

19739 MCFtpRepository for Komitter
	https://pharo.fogbugz.com/f/cases/19739'.
	self loadTogether: self script60412 merge: false.
	self flushCaches.

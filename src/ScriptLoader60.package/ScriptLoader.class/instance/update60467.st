pharo - updates
update60467
	"self new update60467"
	self withUpdateLog: '19943 Error while browsing a comment un MessageBrowser
	https://pharo.fogbugz.com/f/cases/19943

19945 PharoCommandLineHandler class comments incorrect
	https://pharo.fogbugz.com/f/cases/19945

19954 Komitter should explicitly unsubscribe form SystemAnnoucer when window is closed
	https://pharo.fogbugz.com/f/cases/19954

19941 Dark Theme - overlapped title bars need to be distinctive
	https://pharo.fogbugz.com/f/cases/19941'.
	self loadTogether: self script60467 merge: false.
	self flushCaches.

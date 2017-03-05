pharo - updates
update60428
	"self new update60428"
	self withUpdateLog: '19796 Remove duplicate code from SublimishTheme
	https://pharo.fogbugz.com/f/cases/19796

18785 FileList: filter field does not show the actual filter
	https://pharo.fogbugz.com/f/cases/18785

19795 Fix PackageTasksPlugin
	https://pharo.fogbugz.com/f/cases/19795'.
	self loadTogether: self script60428 merge: false.
	self flushCaches.

pharo - updates
update60362
	"self new update60362"
	self withUpdateLog: '17351 Bitmap should have a class initialization
	https://pharo.fogbugz.com/f/cases/17351

18157 ModificationTime creationTime size
	https://pharo.fogbugz.com/f/cases/18157

19613 Problem with read-only objects
	https://pharo.fogbugz.com/f/cases/19613

19537 "Variable used, but not defined anywhere" critique false positive with some slots
	https://pharo.fogbugz.com/f/cases/19537'.
	self loadTogether: self script60362 merge: false.
	self flushCaches.

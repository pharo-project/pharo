pharo - updates
update60317
	"self new update60317"
	self withUpdateLog: '19409 ClassTagRemoved is not announced when packageTag is removed when it becomes empty after class is moved 
	https://pharo.fogbugz.com/f/cases/19409

19319 Editing methods in filtered MessageBrowser raise error
	https://pharo.fogbugz.com/f/cases/19319

19417 Loading new trait methods put them into unclassified category of user classes
	https://pharo.fogbugz.com/f/cases/19417'.
	self loadTogether: self script60317 merge: false.
	self flushCaches.

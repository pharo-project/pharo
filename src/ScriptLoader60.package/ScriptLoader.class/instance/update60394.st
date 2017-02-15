pharo - updates
update60394
	"self new update60394"
	self withUpdateLog: '19010 ImmediateLayout and EphemeronLayout have wrong object format
	https://pharo.fogbugz.com/f/cases/19010'.
	self loadTogether: self script60394 merge: false.
	
ImmediateLayout setFormat: CompiledMethodLayout format.
ImmediateLayout class setFormat: CompiledMethodLayout class format.
EphemeronLayout setFormat: FixedLayout format.
EphemeronLayout class setFormat: FixedLayout class format.
	self flushCaches.

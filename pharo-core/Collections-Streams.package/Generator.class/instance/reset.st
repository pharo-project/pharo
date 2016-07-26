public
reset
	"Reset the generator, i.e., start it over"
	continue ifNotNil:[continue unwindTo: home].
	next := nil.
	continue := thisContext.
	self fork
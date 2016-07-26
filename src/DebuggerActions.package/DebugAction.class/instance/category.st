accessing
category
	"Return the kind of debugger action. It has nothing to do with class category or the old method category now method protocol."
	^ category ifNil: [ self defaultCategory ]
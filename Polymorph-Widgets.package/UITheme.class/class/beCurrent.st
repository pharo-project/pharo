query
beCurrent
	"Make a new instance of the receiver be the current theme."

	self isAbstract ifTrue: [^self error: self name, ' is abstract, send #beCurrent to a subclass.'].
	self current: self newDefault
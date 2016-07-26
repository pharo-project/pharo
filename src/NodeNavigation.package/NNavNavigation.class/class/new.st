instance creation
new
	"yes I'm a singleton"
	Instance ifNil: [Instance := super new].
	^Instance. 
sets support
asSetElement
	"Since nil is a singleton, we need only a single wrapper instance to represent it in set,
	created in advance"
	^ SetElement withNil
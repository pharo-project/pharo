private
ifNoTransformWithIn: box
	"Return true if the current transformation does not affect the given bounding box"
	| delta |
	transform ifNil: [^true].
	delta := (transform localPointToGlobal: box origin) - box origin.
	^(transform localPointToGlobal: box corner) - box corner = delta
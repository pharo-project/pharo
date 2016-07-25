multipart
atomicParts
	"Answer all of the leaf parts of this message, including those of multipart included messages"

	self body isMultipart ifFalse: [^ OrderedCollection with: self].
	^ self parts inject: OrderedCollection new into: [:col :part | col , part atomicParts]
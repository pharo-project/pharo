protected
encodeHeaderWith: anEncoder

	"todo: fix how header is encoded"
	anEncoder encodeByte: (header isEmpty ifTrue: [0] ifFalse: [1]).

	header isEmpty ifFalse: [
		self class newDefault serializationFactory value: header value: anEncoder ]
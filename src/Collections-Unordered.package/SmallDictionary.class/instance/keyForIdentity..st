accessing
keyForIdentity: aValue
	"If aValue is one of the values of the receive, return its key, else return nil.  Contrast #keyAtValue: in which there is only an equality check, here there is an identity check"

	self keysAndValuesDo: [:key :value |  value == aValue ifTrue: [^  key]].
	^ nil
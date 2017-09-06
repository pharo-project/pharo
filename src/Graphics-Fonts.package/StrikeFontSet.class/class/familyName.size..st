instance creation
familyName: aName size: aSize
	"Answer a font (or the default font if the name is unknown) in the specified size."

	| collection |
	collection :=  self allInstances select: [:inst | (inst name beginsWith: aName) and: [inst emphasis = 0]].
	collection isEmpty ifTrue: [
		(aName = 'DefaultMultiStyle') ifTrue: [
			collection := (TextSharedInformation at: #DefaultMultiStyle) fontArray.
		] ifFalse: [
			^ TextStyle defaultFont
		]
	].
	collection := collection asSortedCollection: [:a :b | a pointSize <= b pointSize].
	collection do: [:s | (s pointSize >= aSize) ifTrue: [^ s]].
	^ TextStyle defaultFont.

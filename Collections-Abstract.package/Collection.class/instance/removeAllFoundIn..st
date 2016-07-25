removing
removeAllFoundIn: aCollection 
	"Remove each element of aCollection which is present in the receiver 
	from the receiver. Answer aCollection. No error is raised if an element
	isn't found. ArrayedCollections cannot respond to this message."

	aCollection do: [:each | self remove: each ifAbsent: []].
	^ aCollection
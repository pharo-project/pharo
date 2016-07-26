visual properties
cornerStyle
	"Returns one of the following symbols:
		#square
		#rounded
	according to the current corner style."

	^extension 
		ifNil: [ #square ]
		ifNotNil: [ extension cornerStyle ]
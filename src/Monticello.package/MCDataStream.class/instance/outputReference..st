write and read
outputReference: referencePosn
	"PRIVATE -- Output a reference to the object at integer stream position referencePosn (relative to basePos). To output a weak reference to an object not yet written, supply (self vacantRef) for referencePosn."

	byteStream nextPut: 10. "reference typeID"
	byteStream nextNumber: 4 put: referencePosn	"relative position"
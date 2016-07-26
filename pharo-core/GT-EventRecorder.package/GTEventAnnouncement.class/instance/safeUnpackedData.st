accessing
safeUnpackedData
	"On any error, returns empty collection"
	^ [ self unpackedData ] on: Error do: [ #() ]
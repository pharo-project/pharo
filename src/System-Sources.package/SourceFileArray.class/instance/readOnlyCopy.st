private - copying
readOnlyCopy
	"Answer a read only copy of self. The caller is responsible of closing it."

	^ self species new
		sourcesFileStream: (self sourcesFileStream ifNotNil: [:aStream | aStream readOnlyCopy]);
		changesFileStream: (self changesFileStream ifNotNil: [:aStream | aStream readOnlyCopy]);
		yourself
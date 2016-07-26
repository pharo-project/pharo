private - copying
readOnlyCopy
	"Answer a read only copy of self. The caller is responsible of closing it."

	^ self species new
		sourcesFileStream: (self sourcesFileStream ifNotNil: #readOnlyCopy);
		changesFileStream: (self changesFileStream ifNotNil: #readOnlyCopy);
		yourself
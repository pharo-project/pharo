writing
writeDefinitions: aCollection
	"the correct initialization order is unknown if some classes are missing in the image"

	initializers := Set new.
	self
		acceptVisitor: self forDefinitions: aCollection;
		writePresentInitializers;
		writeAbsentInitializers
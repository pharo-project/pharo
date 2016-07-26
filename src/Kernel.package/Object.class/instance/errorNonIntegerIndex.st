private
errorNonIntegerIndex
	"Create an error notification that an improper object was used as an index."

	self error: 'only integers should be used as indices'
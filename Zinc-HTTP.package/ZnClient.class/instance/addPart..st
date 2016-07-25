accessing request
addPart: mimePart
	"Add mimePart to the multipart/form-data entity of the current request"

	self multiPartFormDataEntity 
		addPart: mimePart 
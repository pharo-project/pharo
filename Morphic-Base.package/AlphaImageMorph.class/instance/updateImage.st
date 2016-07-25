accessing
updateImage
	"Get the image from the model."
	
	(self model notNil and: [self getImageSelector notNil]) ifTrue: [
		(self model perform: self getImageSelector) ifNotNil: [:i | self image: i]]
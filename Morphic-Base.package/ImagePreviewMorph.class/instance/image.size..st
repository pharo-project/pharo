as yet unclassified
image: form size: imageSize
	"Set the image and update the description."
	
	form
		ifNil: [self imageMorph
				image: (self defaultImageFormOfSize: imageSize).
			self textMorph
				contents: '']
	ifNotNil: [self imageMorph
				image: form
				size: imageSize.
			self textMorph
				contents: ('{1} x {2} pixels' translated
							format: {form width asString. form height asString})]
	
comparing
= other

	(self class == ReadWriteStream and: [other class == ReadWriteStream]) ifFalse: [
		^ super = other].	"does an identity test.  Don't read contents of FileStream"
	^ self position = other position and: [self contents = other contents]
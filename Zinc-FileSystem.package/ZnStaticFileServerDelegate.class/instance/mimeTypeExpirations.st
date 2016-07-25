accessing
mimeTypeExpirations
	"Return a dictionary mapping ZnMimeTypes to their expiration time in seconds.
	Missing entries will have no expiration time."

	^ mimeTypeExpirations ifNil: [ mimeTypeExpirations := Dictionary new ]
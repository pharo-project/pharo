private
maxAgeFor: aMimeType
	| expiration |
	expiration := self mimeTypeExpirations at: aMimeType ifAbsent: [ ^ nil ].
	^ 'max-age=', expiration asString
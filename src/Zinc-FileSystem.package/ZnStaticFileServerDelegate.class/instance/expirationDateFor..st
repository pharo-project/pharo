private
expirationDateFor: aMimeType
	| expiration |
	expiration := self mimeTypeExpirations at: aMimeType ifAbsent: [ ^ nil ].
	^ ZnUtils httpDate: (DateAndTime now asUTC + expiration seconds)
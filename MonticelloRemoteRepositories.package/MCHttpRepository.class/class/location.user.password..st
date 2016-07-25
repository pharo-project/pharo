instance creation
location: aUrl user: user password: password

	| result |
	result := self location: aUrl.
	user ifNotEmpty: [
		result
			user: user;
			password: password ].
	^ result.
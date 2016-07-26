private
a1for: username
	| password |
	password := credentials at: username ifAbsent: [ ^ nil ].
	^ self class md5Hash: username, ':', self realm, ':', password
	
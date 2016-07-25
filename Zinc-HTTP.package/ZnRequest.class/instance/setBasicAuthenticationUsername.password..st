accessing
setBasicAuthenticationUsername: username password: password
	(username isNil | password isNil)
		ifTrue: [
			self headers removeKey: 'Authorization' ifAbsent: []]
		ifFalse: [
			self setAuthorization: 'Basic ', (ZnUtils encodeBase64: (username, ':', password)) ]

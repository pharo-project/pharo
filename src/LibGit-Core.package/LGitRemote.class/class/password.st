accessing-credentials
password
	^ self credentials
		at: #password
		ifAbsentPut: [ UIManager default request: 'What''s the password for your account?' ]
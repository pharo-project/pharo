accessing-credentials
username
	^ self credentials
		at: #username
		ifAbsentPut: [ UIManager default request: 'What''s your account name?' ]
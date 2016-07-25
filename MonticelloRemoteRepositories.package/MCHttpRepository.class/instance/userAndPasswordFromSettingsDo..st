actions
userAndPasswordFromSettingsDo: aBlock
	"The mcSettings file in ExternalSettings preferenceDirectory should contain entries for each account:
	
		account1: *myhost.mydomain* user:password
		account2: *otherhost.mydomain/somerep* dXNlcjpwYXNzd29yZA==

	That is it must start with 'account', followed by anything to distinguish accounts, and a colon. Then comes a match expression for the repository url, and after a space the user:password string.
	
	To not have the clear text password on your disc, you can base64 encode it:
			(Base64MimeConverter mimeEncode: 'user:password' readStream) contents
	"

	
	Settings ifNotNil: [
		Settings keysAndValuesDo: [:key :value | | entry userAndPassword |
			(key asLowercase beginsWith: 'account') ifTrue: [
				entry := value findTokens: '	 '.
				(entry first match: self location) ifTrue: [
					userAndPassword := entry second.
					(userAndPassword includes: $:) ifFalse: [
						userAndPassword := (Base64MimeConverter mimeDecodeToChars: userAndPassword readStream) contents].
					userAndPassword := userAndPassword findTokens: $:.
					^aBlock value: userAndPassword first 
						value: userAndPassword second 
					]
			]
		]
	].
	^nil
initialization
wrapBlock: aBlock

	^[ :credAddress :url :username_from_url :allowed_types :data |
		aBlock valueWithArguments: (Array
			with: (LGitCredentials fromHandle: credAddress) 
			with: url
			with: username_from_url
			with: allowed_types 
			with: data) ]
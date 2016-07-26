asserting
assert: registry on: serverUrlString hasUser: nameString withPassword: passwordString 

	registry repositoryAt: serverUrlString credentialsDo: [ :user :password |
		self assert: nameString equals: user.
		self assert: passwordString equals: password ].
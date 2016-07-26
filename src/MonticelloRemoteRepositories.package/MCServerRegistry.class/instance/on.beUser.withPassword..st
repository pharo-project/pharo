public
on: repositoryUrl beUser: nameString withPassword: passwordString

	| credentials |
	credentials := MCServerCredentials user: nameString password: passwordString.
	registry at: repositoryUrl put: credentials.
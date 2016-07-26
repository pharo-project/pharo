repositories
url: urlString username: username password: passwordString
	"Add urlString as a repository for the following package operations."
	| repository |
	repository := MCRepository fromUrl: 
		(urlString asUrl
			username: (username ifEmpty: [ nil ]);
			password: (passwordString ifEmpty: [ nil ]);
			yourself).
	
	self repository: repository.
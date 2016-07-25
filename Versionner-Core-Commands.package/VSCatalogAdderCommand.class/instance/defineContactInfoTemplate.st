metadata methods defintion
defineContactInfoTemplate

	self configurationMetaclass 
		compile: 'catalogContactInfo
	"Returns a paragraph describing contact information such as email, mailing lists and website."
	^ ''MyProject is an amazing project . You can get in contact with our great MyProject programmer at myProject@gmail.com'''
		classified: 'catalog'
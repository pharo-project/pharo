private
loadLatestPackage: aString from: aPath
	| repository |
	
	repository := MCHttpRepository location: aPath.
	self loadLatestPackage: aString fromRepository: repository
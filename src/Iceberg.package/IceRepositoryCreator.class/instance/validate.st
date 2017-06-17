private
validate
	"Validate something is provided to create the repo (I need url OR location OR remote), 
	 but the 3 cannot be nil"
	(self location isNil 
		and: [ self remoteToUse isNil ])
		ifTrue: [ IceError signal: 'No location/url or remote defined.' ].
		
	"Validate if I will add a local repository (url isNil and location isNotNil)... testing the 
	 local directory actually exists"
	(self remoteToUse isNil 
		and: [ self location notNil 
		and: [ self location exists 
		and: [ (self class isGitRoot: self location) not ] ] ])
		ifTrue: [ IceError signal: ('"{1}" is not a git directory' format: { self location fullName }) ]
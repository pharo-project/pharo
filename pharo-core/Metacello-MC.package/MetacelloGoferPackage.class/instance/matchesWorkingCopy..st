private
matchesWorkingCopy: aWorkingCopy
	"check that the working copy package name matches the package file name and that the first ancestor's package file name
	 matches the packageName"
	
	| pFilename |
	(pFilename := self packageFilename) == nil ifTrue: [ ^self error: 'cannot match working copy' ].
	(self class 
		packageFileName: pFilename 
		matchesPackageName: aWorkingCopy package name)
			ifTrue: [
				aWorkingCopy ancestry ancestors isEmpty ifTrue: [ ^true ].
				^self class 
					packageFileName: aWorkingCopy ancestry ancestors first name 
					matchesPackageName: self packageName ].
	^false
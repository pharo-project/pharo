Extension { #name : 'Package' }

{ #category : '*System-Support' }
Package >> allUnsentMessages [

	^ SystemNavigation new
		  environment: self environment;
		  allUnsentMessagesIn: (self methods collect: [ :cm | cm selector ]) asSet
]

{ #category : '*System-Support' }
Package >> isSystemPackage [
	"I return true if I (the package) is a package from the basic Pharo image (this means that it was present in the Pharo image at the end of the bootstrap)."

	^ SystemBuildInfo current systemPackagesIncludes: self
]

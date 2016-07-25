*rpackage-core
packageFromOrganizer: anRPackageOrganizer
	| originSelector |
	"This method returns the package this method belongs to.  
	It takes into account classes and traits.  
	If the method is in no package, returns nil by now"
	self flag: 'TODO: use anRPackageOrganizer, or better delegate to anRPackageOrganizer'.
	originSelector := self originMethod selector.
	^self origin packages 
		detect: [ :each | 
			(each includesSelector: originSelector ofClassName: self origin theNonMetaClass originalName) 
			or: [ each includesSelector: originSelector ofMetaclassName: self origin theNonMetaClass originalName]] 
		ifNone: [ nil ]
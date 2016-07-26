private-convenience
removeModifications
	((Smalltalk globals at: #(Smalltalk globals at: #PackageInfo)) named: self extensionProtocolName) extensionMethods do: [ :methodReference |
		methodReference actualClass removeSelector: methodReference selector ].
	((Smalltalk globals at: #(Smalltalk globals at: #PackageInfo)) named: self hacksCategoryName) in: [ :hacks |
		hacks classes do: [ :classOrTrait |
			classOrTrait removeFromSystem ].
		(Smalltalk globals at: #PackageOrganizer) default  unregisterPackage: hacks ].
	SystemOrganizer default removeCategory: self hacksCategoryName
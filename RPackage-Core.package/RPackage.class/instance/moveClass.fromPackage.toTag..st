private
moveClass: aClass fromPackage: oldPackage toTag: aTag 

	oldPackage removeClass: aClass.
	self importClass: aClass inTag: aTag.
	
	SystemAnnouncer uniqueInstance 
		classRepackaged: aClass 
		from: oldPackage 
		to: self.
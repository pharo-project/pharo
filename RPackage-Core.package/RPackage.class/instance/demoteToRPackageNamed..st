converting
demoteToRPackageNamed: aString 
	| newRPackage |

	self unregister.
	newRPackage := self class organizer 
		packageNamed: aString 
		ifAbsent:  [ (self class named: aString) register ].
	newRPackage importPackage: self.
		
	newRPackage classes do: [ :each | 
		SystemAnnouncer uniqueInstance 
			classRepackaged: each 
			from: self  
			to: newRPackage ].
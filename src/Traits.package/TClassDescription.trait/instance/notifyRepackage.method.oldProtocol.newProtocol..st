private
notifyRepackage: selector method: compiledMethod oldProtocol: oldProtocol newProtocol: newProtocol
	| oldPackage newPackage |
	
	(newProtocol = oldProtocol)
		ifTrue: [ ^ self ].
		
	"This indirection is because we need to abstract RPackage from the kernel"
	self class environment at: #RPackage ifPresent: [ :rPackageClass | 
			newPackage := rPackageClass organizer 
				packageForProtocol: newProtocol 
				inClass: self.
			oldPackage := rPackageClass organizer 
				packageForProtocol: oldProtocol 
				inClass: self.
			
			"Announce recategorization"
			newPackage = oldPackage
				ifFalse: [ 
					SystemAnnouncer uniqueInstance 
						methodRepackaged: compiledMethod 
						from: oldPackage 
						to: newPackage ] ].	
	
	SystemAnnouncer uniqueInstance methodRecategorized: compiledMethod oldProtocol: oldProtocol
private 
asRPackage
	"Create a new RPackage with the same contents as this rpackage tag"
	| newRPackage |
	
	newRPackage := RPackage named: self categoryName.
	self classes 
		do: [ :className | newRPackage importClass: className ].
	(self package extensionsForTag: self) 
		do: [ :extensionMethod | newRPackage addMethod: extensionMethod ].
	
	^ newRPackage
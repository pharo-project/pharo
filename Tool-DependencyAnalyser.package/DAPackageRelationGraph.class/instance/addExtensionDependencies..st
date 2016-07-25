adding
addExtensionDependencies: aPDPackage	
	aPDPackage rPackageSet extensionMethods
		do: [ :method | | packageOfExtendedClass |
			packageOfExtendedClass := self packageForBehavior: method actualClass.
			self addPackage: packageOfExtendedClass.
			aPDPackage
				add:
					((DAExtensionDependency from: aPDPackage to: packageOfExtendedClass)
						theClass: method actualClass;
						selector: method selector asSymbol;
						method: method).
			 ]
testing
testExtensionMethod
	| newMethod newClass newPackage newPackageExt |

	newMethod:= (RGMethodDefinition named: #+)
					isExtension: true.
	self assert: (newMethod protocol isNil).
	self assert: (newMethod isExtension).

	newMethod:= (Collection >> #+) asRingDefinition.	
	self assert: (newMethod isMethod).  
	self assert: (newMethod protocol = '*Collections-arithmetic').
	self assert: (newMethod isExtension).
	
	newPackage := RGPackage named: #Package.
	newPackageExt := RGPackage named:  #PackageExtensions.
	newClass := RGClassDefinition named: #Class.
	newMethod := (RGMethodDefinition named: #foo) parent: newClass.
	newPackage addClass: newClass.
	newMethod package: newPackageExt.
	self assert: (newMethod isExtension).
history
package: aPackage

	self selectedPackage: aPackage.
	
	self announcer announce: (NautilusPackageSelected package: aPackage).
	self ui ifNotNil: [ self ui sourceTextModel announce: NautilusPackageSelected ]
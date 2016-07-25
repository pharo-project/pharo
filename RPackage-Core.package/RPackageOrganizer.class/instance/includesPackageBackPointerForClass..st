private testing
includesPackageBackPointerForClass: aClass

	^ classPackageMapping includesKey: aClass instanceSide name asSymbol
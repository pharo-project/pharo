image package loading
cleanEmptyPackages

	(self packages select:[ :pck| pck classes size isZero and:[ pck methods size isZero ] ])
		do:[ :pck| self removePackage: pck ]
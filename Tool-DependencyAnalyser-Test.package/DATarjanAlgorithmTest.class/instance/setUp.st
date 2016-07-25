running
setUp
	tarjanPackage := DATarjanAlgorithm new.
	anArray := Array new: 8.
	packageA := DAPackage on: (RPackageSet named:'A').
	packageB := (DAPackage on: (RPackageSet named: 'B')).
	packageC := (DAPackage on: (RPackageSet named: 'C')).
	packageD := (DAPackage on: (RPackageSet named: 'D')).
	packageE :=  (DAPackage on: (RPackageSet named: 'E')).
	packageF :=  (DAPackage on: (RPackageSet named: 'F')).
	packageG :=  (DAPackage on: (RPackageSet named: 'G')).
	packageH :=  (DAPackage on: (RPackageSet named: 'H')).
	anArray at: 1 put:packageA.
	anArray at: 2 put:packageB.
	anArray at: 3 put:packageC.
	anArray at: 4 put:packageD.
	anArray at: 5 put:packageE.
	anArray at: 6 put:packageF.
	anArray at: 7 put: packageG.
	anArray at:  8 put:packageH.
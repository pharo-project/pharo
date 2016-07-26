running
testRemoveOutgoingDependencies
	| firstPackage secondPackage outgoingPackage otherOutgoingPackage |
	firstPackage := DAPackage on: (RPackageSet named: 'A').
	secondPackage := DAPackage on: (RPackageSet named: 'B').
	outgoingPackage := DAPackage on: (RPackageSet named: 'C').
	otherOutgoingPackage := DAPackage on: (RPackageSet named: 'D').
	firstPackage add: (DACompositeDependency from: firstPackage to: secondPackage).
	firstPackage add: (DACompositeDependency from: firstPackage to: outgoingPackage).
	firstPackage add: (DACompositeDependency from: firstPackage to: otherOutgoingPackage).
	secondPackage add: (DACompositeDependency from: secondPackage to: firstPackage).
	secondPackage add: (DACompositeDependency from: secondPackage to: outgoingPackage).
	aPDPackageCycle addPackage: firstPackage; addPackage: secondPackage.
	aPDPackageCycle removeOutgoingDependencies.
	"at this point, package A and B does not have dependency with C and D, because C is not in the cycle"
	"self halt."
	self deny: (aPDPackageCycle hasOutgoingDependencies: firstPackage).
	self deny: (aPDPackageCycle hasOutgoingDependencies: secondPackage).
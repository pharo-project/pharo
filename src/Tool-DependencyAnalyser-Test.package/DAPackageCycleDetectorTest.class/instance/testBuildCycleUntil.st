tests
testBuildCycleUntil
	| cycle |
	packageA bfsParent: packageB.
	packageB bfsParent: packageC.
	packageC bfsParent: packageD.
	aPackageCycleDetection buildCycleUntil: packageA.
	self assert: self cyclesSize = 1.
	aPackageCycleDetection buildCycleUntil: packageA.
	"cycle already present"
	self assert: self cyclesSize = 1.
	cycle := aPackageCycleDetection  cycles first.
	self assert: (cycle isInCycle: packageA).
	self assert: (cycle  isInCycle: packageB).
	self assert: (cycle  isInCycle: packageC).
	self assert: (cycle  isInCycle: packageD).
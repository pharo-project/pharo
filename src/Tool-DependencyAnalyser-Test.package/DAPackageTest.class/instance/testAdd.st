tests
testAdd
	| aDependency |
	aDependency := DAPackageDependency from:aPackage to:aSecondPackage.
	aPackage add: aDependency.
	
	self assert: ( (aPackage dependencies size) = 2).
	self assert: ( aPackage dependencies includes: aDependency)
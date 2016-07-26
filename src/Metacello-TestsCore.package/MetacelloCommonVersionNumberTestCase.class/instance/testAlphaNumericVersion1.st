test alpha/numeric version numbers
testAlphaNumericVersion1

	"Use numeric comparison for pure numbers. If you non-numeric version separate with '-'"
	
	| x y |
	self assert: ((x := self versionClass fromString: '2.9.0') < (y := self versionClass fromString: '2.10.0')).

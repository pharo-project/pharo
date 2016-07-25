tests
testRegisterPackageConflictWithPackage
	"self debug: #testRegisteredIsThere"

	| package1 package2 |

	package1 := self createNewPackageNamed: 'P1'.
	package1 register.
	
	package2 := self packageClass named: 'P1'.
	self 
		should: [ package2 register ]
		raise: Error
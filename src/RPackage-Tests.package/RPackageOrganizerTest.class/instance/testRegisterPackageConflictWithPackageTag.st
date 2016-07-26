tests
testRegisterPackageConflictWithPackageTag
	"self debug: #testRegisteredIsThere"

	| package1 package2 |

	package1 := self createNewPackageNamed: 'P1'.
	package1 register.
	package1 addClassTag: #T1.
	
	package2 := self createNewPackageNamed: 'P1-T1'.
	self 
		should: [ package2 register ]
		raise: Error
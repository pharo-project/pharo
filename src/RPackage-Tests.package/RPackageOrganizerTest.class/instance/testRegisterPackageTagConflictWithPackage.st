tests
testRegisterPackageTagConflictWithPackage
	| package1 package2 |

	package1 := self createNewPackageNamed: 'P1-T1'.
	package1 register.	
	
	package2 := self createNewPackageNamed: 'P1'.
	self 
		should: [ package2 addClassTag: #T1 ]
		raise: Error
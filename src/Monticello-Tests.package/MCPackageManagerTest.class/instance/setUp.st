running
setUp
	super setUp.
	package1 := (RPackage named: #A, UUID new asString36) register.
	package2 := (RPackage named: package1 name, #'-SubPart') register.
	mcPackage1 := MCPackage named: package1 name.
	mcPackage2 := MCPackage named: package2 name.
	"register MC packages"
	MCPackageManager
		forPackage: mcPackage1;
		forPackage: mcPackage2
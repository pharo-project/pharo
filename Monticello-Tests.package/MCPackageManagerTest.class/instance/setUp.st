as yet unclassified
setUp
	super setUp.
	package1 := (RPackage named: #A, UUID new asString36) register.
	package2 := (RPackage named: package1 name, #'-SubPart') register.
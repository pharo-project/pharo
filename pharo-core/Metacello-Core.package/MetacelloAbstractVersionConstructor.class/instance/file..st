api
file: aString
    "Define file field of a package spec (MetacelloPackageSpec) or project spec (MetacelloMCProjectSpec).

	For a package spec, the file: field is optional in a baseline. In a baseline, the file field may be used to specify a package branch for the package:
	
		spec package: 'MyPackage' with: [
			spec file: 'MyPackage.gemstone'. ]'.

	The file: field is required in a version. In a version, the file field defines the explicit version of the package to be loaded:
	
		spec package: 'MyPackage' with: [
			spec file: 'MyPackage.gemstone-dkh.1'. ]'.

	The following may be used as a short cut for specifying the file field in a version:

		spec package: 'MyPackage' with: 'MyPackage.gemstone-dkh.1'.

	For a project spec, the file field specifies the name of the Monticello package that contains the configuration. If you are using the convention of 
	naming the class and package usingthe  'ConfigurationOf' prefix, then there is no need to specify the file field:
	
		spec project: 'MyProject' with: [
			spec file: 'ConfigurationMyProject'.

	It should only be used when the package name for the configuration is different from the name of the project:

		spec project: 'MyProject' with: [
			spec file: 'MyProject-Metacello'.
	 "

    self root file: aString constructor: self
intro
introductionText

	^'This tutorial covers some of the methods that can be found in the MetacelloToolBox.
CREATE PROJECT
	1. Create configuration and baseline version
	2. Create development version
	3. Validate and save configuration in project repository
	
DEVELOPMENT
	4. Checkpoint save
	5. Integrate code contributions
	6. Compare versions

RELEASE
	7. Release development version
	8. Create new development version

For convenience, a number of the methods covered in these sections can be found in the ''development support'' category on the class-side of MetacelloBaseConfiguration." 

	Smalltalk tools browser fullOnClass: MetacelloBaseConfiguration class selector: #DevelopmentProcess.

"After deciding which of the development support methods you will use in your project, copy the methods of interest to the the class-side of your own configuration or implement your own ''development support'' methods using the MetacelloToolBox API:"

	HelpBrowser openOn: MetacelloAPIDocumentation
"'
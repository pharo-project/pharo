instance creation
for: aMCClassDefinition 
	^ self new
		targetClassName: aMCClassDefinition className;
		package: (Iceberg packageForCategoryNamed: aMCClassDefinition category)
		yourself
*GT-SpotterExtensions-Core
gtExampleWorkingCopy
	<gtExample>
	<label: 'Metacello working-copy'>
	
	^ (RPackageOrganizer default packageOf: GTSpotter) mcPackage 
		ifNotNil: [ :mcp | mcp workingCopy ]
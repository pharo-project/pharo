*GT-SpotterExtensions-Core
gtExamplePackage
	<gtExample>
	<label: 'Metacello package with corresponding RPackage'>
	
	MCWorkingCopy allManagers do: [ :each | 
		each packageName = 'GT-SpotterExtensions-Core' ifTrue: [ ^ each package ] ].
	^ nil
instance-creation
forArchitecture: architecture

	| candidates |
	candidates := self subclasses select: [ :sc | sc name endsWith: architecture, 'bit' ].
	
	candidates 
		ifEmpty: [ self error: 'No candidates for architecture: ', architecture ].
	candidates size > 1
		ifTrue: [ self error: 'More than one candidate for architecture: ', architecture ].
		
	^ candidates anyOne new
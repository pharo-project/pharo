publishing
publishWarnings
	| body |
	body := String streamContents: [ :str |
		(report warnings sorted: [ :a :b | a package < b package ])
			do: [ :warning | str << warning messageText << '<br/>' ] ].
	self 
		accordion: 'accordionWarnings'
		name: 'Warnings'
		headingContent: 'Warnings ' , (self sizeBadgeFor: report warnings)
		body: body.		
	stream
		<< '</br>'; lf.
	
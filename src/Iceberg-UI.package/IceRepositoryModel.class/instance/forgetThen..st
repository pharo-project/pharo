actions
forgetThen: aBlock
	(UIManager default confirm:
			('Are you sure to remove repository: {1}?
(Packages will not be deleted, nor the repository from the file system)'
				format: { self repository origin url })
		label: 'Removing Iceberg Repository')
	ifTrue: [ 
		self repository forget.
		aBlock cull: self ]

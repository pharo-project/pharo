private
updateRemoteSelectionFrom: aPackage
	| repositories |
	
	aPackage 
		ifNil: [ repositories := #() ]
		ifNotNil: [ repositories := aPackage repositoryGroup repositories ].

	self remotesTree roots do: [ :node |
		self silentWhile: [ 
			node selected: (repositories includes: node content remote) ] ]
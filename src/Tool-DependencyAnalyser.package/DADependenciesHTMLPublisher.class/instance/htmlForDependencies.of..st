publishing
htmlForDependencies: dependencies of: aPackageName
	^ String streamContents: [ :str |
		dependencies
			do: [ :dependency | str 
				<< '<a href="#'
				<< dependency
				<< '">'
				<< (self htmlForDependency: dependency of: aPackageName)
				<< '</a>' ]
			separatedBy: [ str space ] ]
		

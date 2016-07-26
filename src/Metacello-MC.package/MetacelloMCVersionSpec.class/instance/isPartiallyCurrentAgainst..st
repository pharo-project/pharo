testing
isPartiallyCurrentAgainst: resolvedPackageAndProjectNames
	| mcLoader status |
	status := MetacelloMCPartiallyLoadedStatus new.
	mcLoader := self loader.
	self
		specsNamed: resolvedPackageAndProjectNames
		projectDo: [ :prj | 
			| vrsn currentVersion |
			status hasNoProject: false.
			vrsn := prj versionOrNil.
			vrsn ~~ nil
				ifTrue: [ 
					(currentVersion := prj relativeCurrentVersion) ~~ nil
						ifTrue: [ status vrsnStatus add: currentVersion versionStatus ] ].
			currentVersion ~~ nil
				ifTrue: [ 
					status aProjectIsLoaded: true.
					(currentVersion perform: #= with: vrsn)
						ifTrue: [ status aLoadedProjectIsExact: true ]
						ifFalse: [ 
							(currentVersion perform: prj projectReference operator with: vrsn)
								ifTrue: [ status aLoadedProjectIsCurrent: true ]
								ifFalse: [ status aLoadedProjectIsNotCurrent: true ] ] ]
				ifFalse: [ status aProjectNotLoaded: true ] ]
		packageDo: [ :pkg | 
			status hasNoPackage: false.
			pkg
				currentPackageLoaded: [ :versionInfos :file | 
					| wcName wcRef fileRef exact current |
					status aPackageIsLoaded: true.
					versionInfos isEmpty
						ifTrue: [ status aLoadedPackageIsNotCurrent: true ]
						ifFalse: [ 
							exact := current := false.
							versionInfos
								do: [ :vi | 
									wcName := vi name.
									fileRef := GoferResolvedReference name: file.
									wcRef := GoferResolvedReference name: wcName.
									(wcRef compare: fileRef using: #=)
										ifTrue: [ exact := true ] ].
							exact
								ifTrue: [ status aLoadedPackageIsExact: true ]
								ifFalse: [ 
									versionInfos
										do: [ :vi | 
											wcName := vi name.
											fileRef := GoferResolvedReference name: file.
											wcRef := GoferResolvedReference name: wcName.
											(wcRef compare: fileRef using: #>=)
												ifTrue: [ current := true ] ].
									current
										ifTrue: [ status aLoadedPackageIsCurrent: true ]
										ifFalse: [ status aLoadedPackageIsNotCurrent: true ] ] ] ]
				notLoaded: [ status aPackageNotLoaded: true ]
				using: mcLoader ]
		groupDo: [ :ignoredGroup | 
			"if we encounter a group, trouble"
			status abort: true.
			^ status ].
	^ status
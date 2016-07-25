actions
resolveSymbolicVersionsIn: aVersion
	| metacelloToolBox | 
	
	metacelloToolBox := MetacelloToolBox new.
	metacelloToolBox project: aVersion project.
	
	metacelloToolBox
		modifyVersionMethodForVersion: aVersion versionString
		versionSpecsDo: [  :attribute :versionSpec |
			versionSpec 
				specListProjectDo: [ :spec | | projectRef projectRefConfiguration resolvedVersion |
					projectRef := spec projectReference.
					projectRefConfiguration := self 
						projectReference: projectRef name
						configurationFrom: (self baselineFor: aVersion).
					resolvedVersion := (projectRefConfiguration version: projectRef versionString) versionNumber.
					projectRef versionString: resolvedVersion ]
				packageDo: [ :spec ]
				groupDo: [ :spec ].
			true ].
	metacelloToolBox commitMethod
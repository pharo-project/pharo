running
setUp

	(Smalltalk globals includesKey: self configurationName asSymbol) 
		ifFalse: [ 
			MetacelloToolBox configurationNamed: self configurationName.
				(Smalltalk globals at: self configurationName asSymbol) compile: 'baseline10: spec 
        <version: ''1.0-baseline''>
        
        spec for: #common do: [
                spec blessing: #baseline.
                spec repository: ''http://www.squeaksource.com/Versionner''.
		    spec package: ''Collections-Streams''.
                ].'.

			(Smalltalk globals at: self configurationName asSymbol) compile: 'default: spec 
        <version: ''default''>
        
        spec for: #common do: [
                spec blessing: #baseline.
                spec repository: ''http://www.squeaksource.com/Versionner''.
		    spec package: ''Collections-Streams''.
                ].'.
			].
		
	configuration := (Smalltalk globals at: self configurationName asSymbol) 

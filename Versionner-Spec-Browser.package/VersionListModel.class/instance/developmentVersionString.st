private
developmentVersionString
	^ (self listItems 
		detect: [ :item | item isDevelopment ]) versionString
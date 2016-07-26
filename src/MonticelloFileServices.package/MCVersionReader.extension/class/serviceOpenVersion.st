*MonticelloFileServices
serviceOpenVersion
	^ (SimpleServiceEntry
		provider: self
		label: 'Open version'
		selector: #openVersionFromStream:
		description: 'Open a package version'
		buttonLabel: 'Open')
		argumentGetter: [ :fileList | fileList readOnlyStream ]
*MonticelloFileServices
serviceLoadVersion
	^ (SimpleServiceEntry
		provider: self
		label: 'Load version'
		selector: #loadVersionStream:fromDirectory:
		description: 'Load a package version'
		buttonLabel: 'Load')
		argumentGetter: [ :fileList | { fileList readOnlyStream . fileList directory } ]
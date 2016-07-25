*MonticelloFileServices
serviceMergeVersion
	^ (SimpleServiceEntry
		provider: self
		label: 'Merge version'
		selector: #mergeVersionStream:
		description: 'Merge a package version into the image'
		buttonLabel: 'Merge')
		argumentGetter: [ :fileList | fileList readOnlyStream ]
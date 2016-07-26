development support
copySpec: pkgSpec with: gofer to: aRepository

	| repository resolvedReference |
	[resolvedReference := pkgSpec goferLoaderReference resolveWith: gofer]
		on: Error
		do: [:ignored | ^nil ].
	repository := MCRepositoryGroup default repositories
		detect: [ :each | each = aRepository ]
		ifNone: [ aRepository ].
	repository storeVersion: resolvedReference version.
	^resolvedReference repository
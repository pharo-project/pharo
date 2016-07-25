accessing
seenPackagesName
	"Answer all seen packages name."

	^ self seenPackages collect: [ :package | package packageName ]
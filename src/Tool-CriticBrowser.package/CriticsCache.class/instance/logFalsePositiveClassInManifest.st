private
logFalsePositiveClassInManifest
	| manifestBuilder |
	packages
		do: [ :package | 
			manifestBuilder := self builderManifestClass ofPackageNamed: package packageName.
			manifestBuilder rejectClasses \ falsePositiveClasses do: [ :cl | manifestBuilder removeRejectClass: cl ].
			falsePositiveClasses \ manifestBuilder rejectClasses do: [ :cl | manifestBuilder addRejectClass: cl ] ]
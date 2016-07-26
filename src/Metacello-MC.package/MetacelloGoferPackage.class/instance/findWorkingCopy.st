private
findWorkingCopy
	"Answer a working copy, or nil if the package is not loaded."

	| wcs |
	wcs := MCWorkingCopy allManagers select: [ :each | self matchesWorkingCopy: each ].
	wcs isEmpty ifTrue: [ ^nil ].
	^wcs detectMax: [:ea | ea package name size ]
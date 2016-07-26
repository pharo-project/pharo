morphic ui
versionHighlight: aPackageEntry
	"Changed to take in account the fact the version name is in the package entry."

	| verName |
	verName := aPackageEntry info name.
	^ Text string: verName attribute: (self versionHighlightEmphasis: verName)
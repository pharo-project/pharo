accessing
minimalGroupPackageNames

	"return list of all package names for the group named #minimal"

	| packageNames metacelloVersion record|
	packageNames := OrderedCollection new.
	metacelloVersion  := BaselineOfPharoBootstrap project version.
	record := (metacelloVersion ignoreImage: true) record: #minimal.

	record loadDirective packageDirectivesDo: [ :directive |
		packageNames add: directive externalReference packageName.	 ].

	^ packageNames.
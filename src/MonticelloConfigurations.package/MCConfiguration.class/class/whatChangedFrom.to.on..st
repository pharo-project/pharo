utilities
whatChangedFrom: oldConfig to: newConfig on: aStream
	"MCConfiguration
		whatChangedFrom:  ReleaseBuilderPloppDeveloper config20060201PloppBeta
		to:  ReleaseBuilderPloppDeveloper config20060215premaster" 

	| oldDeps |
	oldDeps := Dictionary new.
	oldConfig dependencies do: [:old | oldDeps at: old package put: old].

	newConfig dependencies do: [:new | | old |
		old := oldDeps removeKey: new package ifAbsent: [nil].
		old ifNotNil: [old := old versionInfo].
		self changesIn: new package from: old to: new versionInfo on: aStream.
	].

	oldDeps do: [:old |
		self changesIn: old package from: old versionInfo to: nil on: aStream.
	].

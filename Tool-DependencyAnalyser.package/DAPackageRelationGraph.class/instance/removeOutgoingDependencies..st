private
removeOutgoingDependencies: aPackage
	| removals |
	removals := aPackage dependencies reject: [ :each | each target isIncluded ].
	removals do: [ :each | aPackage remove: each ]
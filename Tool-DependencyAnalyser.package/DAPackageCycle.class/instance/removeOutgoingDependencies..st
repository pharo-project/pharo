private
removeOutgoingDependencies: aPackage
	
	| removals |
	packages := packages copy.
	removals := aPackage dependencies reject: [ :dependency | self packages includes: (dependency target) ].
	
	removals do: [ :each | aPackage remove: each ].
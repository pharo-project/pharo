visiting
pushProject: aBlock

	| oldGroups oldPackages oldProjects |
	oldGroups := groups.
	oldPackages := packages.
	oldProjects := projects.
	groups := Set new.
	packages := Set new.
	^aBlock ensure: [
		groups := oldGroups.
		packages := oldPackages.
		projects := oldProjects ]
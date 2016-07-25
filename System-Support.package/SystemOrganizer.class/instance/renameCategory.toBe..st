accessing
renameCategory: oldCatString toBe: newCatString
	| r |
	r := super renameCategory: oldCatString toBe: newCatString.
	SystemAnnouncer uniqueInstance 
		classCategoryRenamedFrom: oldCatString to: newCatString.
	^ r
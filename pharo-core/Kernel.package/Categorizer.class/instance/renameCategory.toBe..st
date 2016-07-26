operations
renameCategory: oldCatString toBe: newCatString
	"Rename a category. No action if new name already exists, or if old name does not exist."
	| index oldCategory newCategory |
	oldCategory := oldCatString asSymbol.
	newCategory := newCatString asSymbol.
	(categoryArray indexOf: newCategory) > 0
		ifTrue: [^ self].	"new name exists, so no action"
	(index := categoryArray indexOf: oldCategory) = 0
		ifTrue: [^ self].	"old name not found, so no action"
	categoryArray := categoryArray copy.  "need to change identity so smart list update will notice the change"
	categoryArray at: index put: newCategory
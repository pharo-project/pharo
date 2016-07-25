private
renameCategory: oldCatString toBe: newCatString
	| cls |
	cls := self model selectedClass.
	cls organization renameCategory: oldCatString toBe: newCatString.
	self deselectProtocol: oldCatString asSymbol.
	self model selectedCategory: newCatString asSymbol.
	self selectProtocol: newCatString asSymbol.
	self model updateCategoryAndMethodList
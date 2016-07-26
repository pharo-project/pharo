tests extensions
testSilentlyRenameCategoryToBe

	| class |
	
	class := self createNewClassNamed: #ClassForTestAboutSilentRecategorization inCategory: 'RPackage-Tests'.
	class organization addCategory:#foo.
	self assert: (class organization categories includes: #foo).
	self deny: (class organization categories includes: #bar).
	class organization silentlyRenameCategory: #foo toBe: #bar.
	
	self assert: (class organization categories includes: #bar).
	self deny: (class organization categories includes: #foo).	
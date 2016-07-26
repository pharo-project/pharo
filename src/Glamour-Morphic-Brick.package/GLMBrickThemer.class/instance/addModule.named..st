brick-modules
addModule: aThemerModule named: aSymbol

	self modules at: aSymbol put: aThemerModule.
	aThemerModule themer: self.
	^ aThemerModule
compatibility tests
testMoveTo
	| old new |
	[
		old := FileLocator imageDirectory / 'testMoveTo_old'.
		old ensureCreateFile.
		
		new := FileLocator home / 'testMoveTo_new'.
		old moveTo: new.
		
		self deny: old exists.
		self assert: new exists.
	] ensure: [ 
		old ensureDelete.
		new ensureDelete.
	]
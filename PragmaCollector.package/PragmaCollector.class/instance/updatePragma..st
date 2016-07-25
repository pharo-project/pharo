updating
updatePragma: aPragma
	"only announce that the pragma has been updated"
	self announcer announce: (PragmaUpdated pragma: aPragma)
updating
removePragma: aPragma 
	"remove an handler an announce it"
	self collected remove: aPragma ifAbsent: [].
	self announce: (PragmaRemoved pragma: aPragma)
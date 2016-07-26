initialize-release
scope: level
	"Note scope of temporary variables.
	Currently only the following distinctions are made:
		0	outer level: args and user-declared temps
		1	block args and doLimiT temps
		-1	a block temp that is no longer active
		-2	a block temp that held limit of to:do:"
	scope := level
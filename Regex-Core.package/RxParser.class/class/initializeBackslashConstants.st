class initialization
initializeBackslashConstants
	"self initializeBackslashConstants"

	(BackslashConstants := Dictionary new)
		at: $e put: Character escape;
		at: $n put: Character lf;
		at: $r put: Character cr;
		at: $f put: Character newPage;
		at: $t put: Character tab
class initialization
initializeEscapedLetterSelectors
	"self initializeEscapedLetterSelectors"

	(EscapedLetterSelectors := Dictionary new)
		at: $w put: #beWordConstituent;
		at: $W put: #beNotWordConstituent;
		at: $d put: #beDigit;
		at: $D put: #beNotDigit;
		at: $s put: #beSpace;
		at: $S put: #beNotSpace;
		at: $\ put: #beBackslash
class initialization
initializeBackslashSpecials
	"Keys are characters that normally follow a \, the values are
	associations of classes and initialization selectors on the instance side
	of the classes."
	"self initializeBackslashSpecials"

	(BackslashSpecials := Dictionary new)
		at: $w put: (Association key: RxsPredicate value: #beWordConstituent);
		at: $W put: (Association key: RxsPredicate value: #beNotWordConstituent);
		at: $s put: (Association key: RxsPredicate value: #beSpace);
		at: $S put: (Association key: RxsPredicate value: #beNotSpace);
		at: $d put: (Association key: RxsPredicate value: #beDigit);
		at: $D put: (Association key: RxsPredicate value: #beNotDigit);
		at: $b put: (Association key: RxsContextCondition value: #beWordBoundary);
		at: $B put: (Association key: RxsContextCondition value: #beNonWordBoundary);
		at: $< put: (Association key: RxsContextCondition value: #beBeginningOfWord);
		at: $> put: (Association key: RxsContextCondition value: #beEndOfWord)
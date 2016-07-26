class initialization
initialize
	enabled := true.
	overrideModel := false.
	caseSensitive := true.
	smartCharacters := true.
	expandPrefixes := true.
	captureNavigationKeys := false.
	useEnterToAccept := true.
	smartCharactersMapping := Dictionary new.
	smartCharactersMapping
		at: $( put: $);
		at: $[ put: $];
		at: ${ put: $};
		at: $" put: $";
		at: $' put: $'.
	smartCharactersWithSingleSpace := ''.
	smartCharactersWithDoubleSpace := '[]{}'.
	backgroundColor := Smalltalk ui theme settings menuColor ifNil: [ Color white ].
	popupShowWithShortcut := self defaultPopupShortcut.
	popupShowAutomatic := self defaultPopupShowAutomatic.
	popupAutomaticDelay := self defaultPopupDelay.
	spaceAfterCompletion := true
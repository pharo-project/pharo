error correction
declareTempAndPaste: name
	| insertion delta theTextString characterBeforeMark |

	theTextString := requestor text string.
	characterBeforeMark := theTextString at: tempsMark-1 ifAbsent: [$ ].
	(theTextString at: tempsMark) = $| ifTrue: [
  		"Paste it before the second vertical bar"
		insertion := name, ' '.
		characterBeforeMark isSeparator ifFalse: [insertion := ' ', insertion].
		delta := 0.
	] ifFalse: [
		"No bars - insert some with CR, tab"
		insertion := '| ' , name , ' |',String cr.
		delta := 2.	"the bar and CR"
		characterBeforeMark = Character tab ifTrue: [
			insertion := insertion , String tab.
			delta := delta + 1.	"the tab"
		].
	].
	tempsMark := tempsMark +
		(self substituteWord: insertion
			wordInterval: (tempsMark to: tempsMark-1)
			offset: 0) - delta.
	^ encoder bindAndJuggle: name
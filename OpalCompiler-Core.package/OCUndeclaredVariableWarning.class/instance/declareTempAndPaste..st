correcting
declareTempAndPaste: name
	| insertion delta theTextString characterBeforeMark tempsMark newMethodNode |

	"Below we are getting the text that is actually seen in the morph. This is rather ugly. Maybe there is a better way to do this."
	theTextString := self requestor textMorph editor paragraph text.

	"We parse again the method displayed in the morph. The variable methodNode has the first version of the method, without temporary declarations. "
	newMethodNode := RBParser parseMethod: theTextString.
	
	"We check if there is a declaration of temporary variables"
	tempsMark :=  newMethodNode body  rightBar ifNil: [ self methodNode body start ].	
	
	characterBeforeMark := theTextString at: tempsMark-1 ifAbsent: [$ ].

	(theTextString at: tempsMark) = $| ifTrue:  [
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
			delta := delta + 1.	"the tab" ]
		].
	tempsMark := tempsMark +
		(self substituteWord: insertion
			wordInterval: (tempsMark to: tempsMark-1)
			offset: 0) - delta.
	" we can not guess at this point where the tempvar should be stored,
	tempvars vs. tempvector therefore -> reparse"
	(OCSourceCodeChanged new newSourceCode: self requestor text) signal
copying
appendCopyTo: anotherText

	"Create a copy of receiver and append it to the another text (at the end of it).
	Answer the copy"

	| copy |
	
	copy := self copy.
	
	copy 
		text: anotherText;
		basicNext: copy.
	
	anotherText endSpan prev insertNext: copy.
	
	^ copy
	
copying
appendCopyTo: anotherText
	"Create a copy of receiver and append it to the another text (at the end of it).
	Answer the copy"

	"End span cannot be copied, instead we just answer the existing end span of another text"
	^ anotherText endSpan
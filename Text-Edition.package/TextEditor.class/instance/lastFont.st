accessing
lastFont	
	"Answer the Font for to be used if positioned at the end of the text"
	| t |	
	t := self text.	
	^ t fontAt: t size + 1
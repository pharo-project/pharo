accessing
lineNumberAt: textPosition 
	"i am not changing the number of lines"
	^ self numberOfLinesBefore + (textPosition spanPosition "0 or 1")
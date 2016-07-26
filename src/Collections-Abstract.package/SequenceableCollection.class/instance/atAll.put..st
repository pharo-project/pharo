accessing
atAll: aCollection put: anObject 
	"Put anObject at every index specified by the elements of aCollection."

	aCollection do: [:index | self at: index put: anObject].
	^ anObject
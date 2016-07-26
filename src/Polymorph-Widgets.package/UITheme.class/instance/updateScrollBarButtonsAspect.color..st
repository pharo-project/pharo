scrollbars
updateScrollBarButtonsAspect: aCollection color: aColor 
	"update aCollection of morphs with aColor"
	
	
	aCollection
		do: [:each | self updateScrollBarButtonAspect: each color: aColor]
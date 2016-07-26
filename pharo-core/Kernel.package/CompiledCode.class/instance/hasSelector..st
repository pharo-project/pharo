literals
hasSelector: selector 
	"Answers true if the method refers to the selector"
	^ self hasSelector: selector specialSelectorIndex: (Smalltalk specialSelectorIndexOrNil: selector)
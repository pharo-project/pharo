accessing
wheelDirection
	"If button is wheel button (>3), answer direction (#up/#down)"
	^ self button = 4 
		ifTrue: [ #up ] 
		ifFalse: [ #down ]
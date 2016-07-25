api
isDisplayed
	"Return true if the widget is currently displayed on screen"
	
	^ self window notNil and: [ self window isDisplayed ]
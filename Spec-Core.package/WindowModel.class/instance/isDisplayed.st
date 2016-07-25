api
isDisplayed

	^ self window ifNil: [ false ] ifNotNil: [ :theWindow | theWindow isDisplayed ]
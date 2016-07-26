accessing
contents
	^ contents ifNil: [ contents := self allContents select: [ :each | each shouldBeDisplayed ] ]
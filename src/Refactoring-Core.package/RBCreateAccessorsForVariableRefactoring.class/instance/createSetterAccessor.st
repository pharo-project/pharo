transforming
createSetterAccessor
	setterMethod := self findSetterMethod.
	setterMethod isNil ifTrue: [setterMethod := self defineSetterMethod]
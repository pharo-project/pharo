transforming
createGetterAccessor
	getterMethod := self findGetterMethod.
	getterMethod isNil ifTrue: [getterMethod := self defineGetterMethod]
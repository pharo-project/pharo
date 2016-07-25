private
reportBogusBehaviorOf: aSelector

	self logCr: 'RPackage log: Something wrong around ', aSelector asString , 'since the removeKey: is called on not present information.'.
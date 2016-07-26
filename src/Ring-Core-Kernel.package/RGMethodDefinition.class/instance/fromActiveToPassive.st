type of methods
fromActiveToPassive
	"If the receiver was generated as an active method, it can be converted to a passive one by reading the data of the compiled method (if exists)"

	| compiledMethod |
	self isActive
		ifFalse: [ ^ self ].
	compiledMethod := self compiledMethod.
	compiledMethod notNil
		ifTrue: [ 
			self protocol: compiledMethod category.
			self sourceCode: compiledMethod sourceCode.
			self stamp: compiledMethod timeStamp ].
	self asPassive
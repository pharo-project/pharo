adding/removing methods
basicAddSelector: selector withMethod: compiledMethod 
	"Add the message selector with the corresponding compiled method to the 
	receiver's method dictionary.
	Do this without sending system change notifications"

	| oldMethodOrNil |	
	oldMethodOrNil := self lookupSelector: selector.
	self methodDict at: selector put: compiledMethod.
	compiledMethod methodClass: self.
	compiledMethod selector: selector.

	"Now flush Pharo's method cache, either by selector or by method"
	oldMethodOrNil ifNotNil: [oldMethodOrNil flushCache].
	selector flushCache.
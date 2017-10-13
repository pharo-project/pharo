error handling
handle: anException about: anEntity forProppertiesDo: aBlock
	"do something with anException that occured during anEntity validation.
	in case you want to produce a new renraku property evaluate aBlock with it"
	
	self subclassResponsibility 
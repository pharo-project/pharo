primitives
externalType: descriptorClass
	"Parse an return an external type"
	| xType |
	xType := descriptorClass atomicTypeNamed: here.
	xType == nil ifTrue:["Look up from class scope"
		Symbol hasInterned: here ifTrue:[:sym|
			xType := descriptorClass structTypeNamed: sym]].
	xType == nil ifTrue:[
		"Raise an error if user is there"
		self interactive ifTrue:[^nil].
		"otherwise go over it silently"
		xType := descriptorClass forceTypeNamed: here].
	self advance.
	(self matchToken:#*)
		ifTrue:[^xType asPointerType]
		ifFalse:[^xType]
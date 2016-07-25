public menu building
menuSpecAt: aName
	"returns a PluggableMenuSpec build from my contents starting at 
	the inner MenuRegistration named aName or from here if aName is nil"
	| root |
	self buildTree.
	root := PluggableMenuSpec withModel: nil.
	(aName ifNil: [self] ifNotNil: [self itemNamed: aName]) 
		ifNotNil: [:top | top precondition value ifTrue: [top buildMenuSpec: root]].
	^ root
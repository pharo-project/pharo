*Polymorph-Widgets
visible: aBoolean 
	"set the 'visible' attribute of the receiver to aBoolean.
	Must update owner layout since its full bounds may depend
	on the receiver extending beyond its bounds."
	
	(extension isNil and:[aBoolean]) ifTrue: [^ self].
	self visible == aBoolean ifTrue: [^ self].
	self assureExtension visible: aBoolean.
	self changed.
	owner ifNotNil: [owner layoutChanged]
accessing
lock: aBoolean 
	"change the receiver's lock property"
	(extension isNil and: [aBoolean not]) ifTrue: [^ self].
	self assureExtension locked: aBoolean
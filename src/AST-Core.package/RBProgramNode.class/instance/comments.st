accessing
comments
	"Answer the comments of the receiving parse tree node"
	
	^ (self propertyAt: #comments ifAbsent: [ #() ]) ifNil:[ #() ]
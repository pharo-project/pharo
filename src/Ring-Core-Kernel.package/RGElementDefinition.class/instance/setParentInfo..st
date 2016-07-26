private
setParentInfo: anObject
	"anObject is aRGBehaviorDefinition or aClass/aTrait"

	self parentName: anObject name. 
	self isMetaSide: anObject isMeta
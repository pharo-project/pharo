accessing
model: aDomainObject

	"aDomainObject can be regular object, a value holder (NewValueHolder) or an instance of Model"
	
	(aDomainObject isAnnouncingObject not and: [ self announcingObject isValueHolder ])
		ifTrue: [ self announcingObject value: aDomainObject ]
		ifFalse: [ self setAnnouncingObject: aDomainObject ]
	
accessing
model: aDomainObject

	"aDomainObject can be regular object, a value holder (NewValueHolder) or an instance of Model"
	
	(aDomainObject isSpecCompatibleModel not and: [ self specCompatibleModel isValueHolder ])
		ifTrue: [ self specCompatibleModel value: aDomainObject ]
		ifFalse: [ self specCompatibleModel: aDomainObject ]
	
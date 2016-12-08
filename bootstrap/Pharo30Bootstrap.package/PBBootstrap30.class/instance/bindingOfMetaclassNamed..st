accessing
bindingOfMetaclassNamed: aBehaviorName

	^remoteMetaclassBindings
		at: aBehaviorName
		ifAbsentPut: [ objectSpace
			newAssociationWithKey: objectSpace nilObject
			andValue: (bootstrapEnvironment at: aBehaviorName) basicClass ]
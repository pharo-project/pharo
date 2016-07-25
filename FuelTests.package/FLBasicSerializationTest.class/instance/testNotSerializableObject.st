tests-not-so-basic
testNotSerializableObject

	self 
		should: [ self serialize: FLNotSerializableMock new ] 
		raise: FLNotSerializable
		whoseDescriptionIncludes: FLNotSerializableMock new printString
		description: 'User can prevent some objects from serialization.'
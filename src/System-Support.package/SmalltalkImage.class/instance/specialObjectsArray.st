special objects
specialObjectsArray
	^specialObjectsArray ifNil: [ specialObjectsArray := self primitiveGetSpecialObjectsArray ].
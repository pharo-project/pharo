visitor
stringForBehavior: aClassOrTrait

	^ String streamContents: [ :stream |
		stream
			nextPutAll: aClassOrTrait definitionSource;
			nextPut: $.;
			cr; cr;
			nextPutAll: aClassOrTrait theMetaClass definitionSource ]
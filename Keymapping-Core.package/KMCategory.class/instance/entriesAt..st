accessing
entriesAt: aPlatform
	platforms ifNil: [ platforms := Dictionary new ].
	^platforms at: aPlatform ifAbsentPut: [ KMStorage new ].
assertions
assertSelector: aSelector collects: aCollection from: aDictionary
	| collected |

	collected := aDictionary perform: aSelector.
	self
		assert: collected class == self collectClass;
		assert: collected asArray = aCollection asArray.
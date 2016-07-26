accessing-properties
ensureProperties
	^ TraitProperties at: self ifAbsentPut: WeakKeyDictionary new
		
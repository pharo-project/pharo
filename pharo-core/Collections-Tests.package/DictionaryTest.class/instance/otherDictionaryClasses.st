requirements
otherDictionaryClasses
	"(DictionaryTest withAllSubclasses collect: [:each | each new classToBeTested]) asArray"
	^ {SmallDictionary. IdentityDictionary. LiteralDictionary. PluggableDictionary.
		WeakValueDictionary. SystemDictionary. WeakKeyDictionary. SmallIdentityDictionary.
		WeakIdentityKeyDictionary. WeakKeyToCollectionDictionary. Dictionary}
			reject: [:each | each new species == self classToBeTested new species]
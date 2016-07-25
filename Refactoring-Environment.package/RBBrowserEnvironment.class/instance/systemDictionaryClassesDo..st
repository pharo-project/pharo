private
systemDictionaryClassesDo: aBlock
	self systemDictionary allClassesDo:[:class |
		 aBlock value: class; value: class theMetaClass]
cache accessing
valueListFor: aSettingValue
	| targetEntry |
	targetEntry := self valueListCache at: aSettingValue settingReceiver ifAbsentPut: [IdentityDictionary new].
	^ targetEntry at: aSettingValue getSelector ifAbsentPut: [OrderedCollection new].
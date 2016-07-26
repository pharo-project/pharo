tests - loading
testStoreAndLoadAllSystemSettings
	| searchCompiledMethod |
	systemSettings settingTree: SettingBrowser new treeHolder.
	systemSettings storeSettingNodes.
	systemSettings allStoredSettings do: #realValue.
	searchCompiledMethod := [ :aClass :aBlock | aClass compiledMethodAt: #= ifAbsent: [ aBlock value: aClass superclass value: aBlock ] ].
	systemSettings allStoredSettings do: [ :eachStoredSetting | | settingNode compiledMethod |
		settingNode := systemSettings settingNodeForStoredSetting: eachStoredSetting.
		self assert: eachStoredSetting realValue class equals: settingNode realValue class.
		compiledMethod := searchCompiledMethod value: eachStoredSetting realValue class value: searchCompiledMethod.
		compiledMethod methodClass == Object ifFalse: [ 
			self assert: eachStoredSetting realValue equals: settingNode realValue ] ].
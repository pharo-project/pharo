tests
testUniqueNames
	| names |
	names := self systemSettingNodeList collect: [ :eachNode | 
						eachNode parentName printString, eachNode item name ].
	self assert: names size equals: names asSet size.
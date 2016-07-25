tests - loading
testWriteAndReadBoolean
	| node stored string stored2 |
	node := MockSettings booleanSettingNode.
	stored := StoredSettingsFactory new from: node.
	self assert: stored notNil.
	string := String streamContents: [ :aStream | | writer |
		writer := STON writer on: aStream; prettyPrint: true; yourself.
		writer writeObject: stored.
	].
	self assert: string notNil.
	stored2 := STON fromString: string. 
	self assert: stored2 notNil.
	self assert: stored2 equals: stored.
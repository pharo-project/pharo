testing
testRevertOverrideMethod
	| definition |
	self class compile: 'override ^ 2' classified: self mockOverrideMethodCategory.
	definition := (RGMethodDefinition realClass: self class selector: #override) asMCMethodDefinition.
	self assert: definition isOverrideMethod.
	self assert: self override = 2.
	definition unload.
	self assert: self override = 1.
	self assert: (RGMethodDefinition realClass: self class selector: #override) category = 'mocks'.
	
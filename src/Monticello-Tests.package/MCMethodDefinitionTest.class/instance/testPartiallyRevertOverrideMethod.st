testing
testPartiallyRevertOverrideMethod
	| definition |
	self class compile: 'override ^ 2' classified: '*foobarbaz'.
	self class compile: 'override ^ 3' classified: self mockOverrideMethodCategory.
	self class compile: 'override ^ 4' classified: self mockOverrideMethodCategory.
	definition := (RGMethodDefinition realClass: self class selector: #override) asMCMethodDefinition.
	self assert: definition isOverrideMethod.
	self assert: self override = 4.
	definition unload.
	self assert: self override = 2.
	self assert: (RGMethodDefinition realClass: self class selector: #override) category = '*foobarbaz'.
	
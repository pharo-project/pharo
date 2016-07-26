tests
testComposite
	changes 
		compile: 'method ^ 1' in: self class classified: #utilities;
		compile: 'method ^ 2' in: self class class classified: #utilities.
	self assert: (changes changesSize = 2).
	self assert: (changes problemCount = 2).
	self assert: (changes changeForClass: self class name selector: #method) notNil.
	self assert: (changes changeForMetaclass: self class name selector: #method) notNil.
	self universalTestFor: changes
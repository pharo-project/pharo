tests
testEmptyTextInput
	self testNewSpotter.
	
	self assertText: Text new do: [ self assert: spotter text isEmpty ].
	self assertText: (Text fromString: '') do: [ self assert: spotter text isEmpty ].
	self assertText: '' do: [ self assert: spotter text isEmpty ].
	self flag: 'currently: nil is converted to the string "nil"'
	"self assertText: nil do: [ self assert: spotter text isEmpty ]."
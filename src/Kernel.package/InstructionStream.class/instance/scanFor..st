scanning
scanFor: scanBlock
	"Check all bytecode instructions with scanBlock, answer true if scanBlock answers true.
	This can be used to, e.g., check whether a method contains 'push closure' bytecodes like this:
	aMethod scanFor: [ :b | b = 143 ]"

	| method encoderClass end byte |
	method := self method.
	end := method endPC.
	encoderClass := method encoderClass.
	[pc <= end] whileTrue: 
		[(scanBlock value: (byte := method at: pc)) ifTrue:
			[^true].
		 pc := pc + (encoderClass bytecodeSize: byte)].
	^false
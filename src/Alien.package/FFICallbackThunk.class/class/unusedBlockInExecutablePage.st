executable page management
unusedBlockInExecutablePage
	AccessProtect critical:
		[ExecutablePages do:
			[:alien|
			1 to: alien dataSize - self dataSize by: self dataSize do:
				[:i|
				(alien unsignedByteAt: i) = 0 ifTrue:
					[alien unsignedByteAt: i put: 1.
					 ^alien address + i - 1]]]].
	^(AccessProtect critical: [ExecutablePages add: self allocateExecutablePage])
		unsignedByteAt: 1 put: 1;
		address
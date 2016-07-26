private
copyFromMatrix: m
	self assert: [ m getHandle ~= self getHandle ].
	LibC memCopy: m getHandle to: self getHandle size: self class byteSize
accessing
callType
	self callingConvention = #cdecl ifTrue: [ ^ 0 ].
	self callingConvention = #stdcall ifTrue: [ ^ 1 ].

	self error: 'Invalid call convention!'
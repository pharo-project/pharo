accessing
callType: aCallType
	aCallType == #cdecl ifTrue: [ ^ self cdecl ].
	aCallType == #stdcall ifTrue: [ ^ self stdcall ].
	
	self error: 'unknown call type'
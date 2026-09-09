Extension { #name : 'WinPlatform' }

{ #category : '*FFI-Kernel' }
WinPlatform >> getErrorMessage: anInteger [

    | buffer |
    buffer := (String new: 500) asWin32WideString.
    self formatMessage: 16r1000 "Format Message From System"
                        _: ExternalAddress null
                        _: anInteger
                        _: 0
                        _: buffer
                        _: 500
                        _: ExternalAddress null.

    ^ buffer asString
]

{ #category : '*FFI-Kernel' }
WinPlatform >> getTempPath [

  | buffer length |
	length := self defaultMaximumPathLength.
	buffer := (String new: length) asWin32WideString.
	self getTempPath: length buffer: buffer.
	^buffer asString
]

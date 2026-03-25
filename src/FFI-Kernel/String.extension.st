Extension { #name : 'String' }

{ #category : '*FFI-Kernel' }
String >> asWin32WideString [

	^ Win32WideString fromString: self
]

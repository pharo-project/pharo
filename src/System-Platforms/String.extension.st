Extension { #name : #String }

{ #category : #'*System-Platforms-Windows' }
String >> asWin32WideString [

	^ Win32WideString fromString: self
]

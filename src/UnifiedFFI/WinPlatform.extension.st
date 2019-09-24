Extension { #name : #WinPlatform }

{ #category : #'*UnifiedFFI' }
WinPlatform >> ffiCallingConvention [
	^ #stdcall
]

{ #category : #'*UnifiedFFI' }
WinPlatform >> ffiFloat64Alignment [
	"Windows structure double alignment is 8. 
	 Check here: https://msdn.microsoft.com/en-us/library/aa290049(v=vs.71).aspx"
	^ 8
]

{ #category : #'*UnifiedFFI' }
WinPlatform >> ffiLibraryName: aLibrary [ 
	^ aLibrary win32LibraryName
]

{ #category : #'*UnifiedFFI' }
WinPlatform >> ffiLongAlignment [
	"In Windows, sizeof(long) == 4 always."
	^ 4
]

misc
loadSymbolFrom: hModule name: lpProcName
	^ self ffiCall: #(void *GetProcAddress(void *hModule, String lpProcName))

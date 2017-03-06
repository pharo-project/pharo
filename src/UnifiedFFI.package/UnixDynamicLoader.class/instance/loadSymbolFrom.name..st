misc
loadSymbolFrom: handle name: symbol
	^ self ffiCall: #(void *dlsym(void *handle, String symbol))

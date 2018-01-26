Extension { #name : #Object }

{ #category : #'*UnifiedFFI-Legacy' }
Object >> nbCall: fnSpec [
	" you can override this method if you need to"
	<ffiCalloutTranslator>
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		function: fnSpec module: self ffiLibraryName
]

{ #category : #'*UnifiedFFI-Legacy' }
Object >> nbCall: fnSpec module: aModuleNameOrHandle [
	" you can override this method if you need to"
	<ffiCalloutTranslator>
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		function: fnSpec module: aModuleNameOrHandle
]

{ #category : #'*UnifiedFFI-Legacy' }
Object >> nbCall: fnSpec module: aModuleNameOrHandle options: callOptions [
	" you can override this method if you need to"
	<ffiCalloutTranslator>
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: callOptions;
		function: fnSpec module: aModuleNameOrHandle
]

{ #category : #'*UnifiedFFI-Legacy' }
Object >> nbCall: fnSpec options: callOptions [
	" you can override this method if you need to"
	<ffiCalloutTranslator>

	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: callOptions;
		function: fnSpec module: self ffiLibraryName
]

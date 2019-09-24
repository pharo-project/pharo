Extension { #name : #Object }

{ #category : #'*UnifiedFFI-Legacy' }
Object >> nbCall: fnSpec [
	"You can override this method if you need to"
	<ffiCalloutTranslator>
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		function: fnSpec library: self ffiLibraryName
]

{ #category : #'*UnifiedFFI-Legacy' }
Object >> nbCall: fnSpec module: aModuleNameOrHandle [
	"You can override this method if you need to"
	<ffiCalloutTranslator>
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		function: fnSpec library: aModuleNameOrHandle
]

{ #category : #'*UnifiedFFI-Legacy' }
Object >> nbCall: fnSpec module: aModuleNameOrHandle options: callOptions [
	"You can override this method if you need to"
	<ffiCalloutTranslator>
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: callOptions;
		function: fnSpec library: aModuleNameOrHandle
]

{ #category : #'*UnifiedFFI-Legacy' }
Object >> nbCall: fnSpec options: callOptions [
	"You can override this method if you need to"
	<ffiCalloutTranslator>

	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: callOptions;
		function: fnSpec library: self ffiLibraryName
]

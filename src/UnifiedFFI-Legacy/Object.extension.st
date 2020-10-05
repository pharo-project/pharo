Extension { #name : #Object }

{ #category : #'*UnifiedFFI-Legacy' }
Object >> nbCall: fnSpec [
	"You can override this method if you need to"
	<ffiCalloutTranslator>
	self
		deprecated: 'use ffiCall: instead'
		transformWith: '`@rec nbCall: `@arg' -> '`@rec ffiCall: `@arg'.
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		function: fnSpec library: self ffiLibraryName
]

{ #category : #'*UnifiedFFI-Legacy' }
Object >> nbCall: fnSpec module: aModuleNameOrHandle [
	"You can override this method if you need to"
	<ffiCalloutTranslator>
	self
		deprecated: 'use ffiCall:module: instead'
		transformWith: '`@rec nbCall: `@arg1 module: `@arg2' -> '`@rec ffiCall: `@arg1 module: `@arg2'.
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		function: fnSpec library: aModuleNameOrHandle
]

{ #category : #'*UnifiedFFI-Legacy' }
Object >> nbCall: fnSpec module: aModuleNameOrHandle options: callOptions [
	"You can override this method if you need to"
	<ffiCalloutTranslator>
	self
		deprecated: 'use ffiCall:module:options: instead'
		transformWith: '`@rec nbCall: `@arg1 module: `@arg2 options: `@arg3' -> '`@rec ffiCall: `@arg1 module: `@arg2 options: `@arg3'.
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: callOptions;
		function: fnSpec library: aModuleNameOrHandle
]

{ #category : #'*UnifiedFFI-Legacy' }
Object >> nbCall: fnSpec options: callOptions [
	"You can override this method if you need to"
	<ffiCalloutTranslator>
	self
		deprecated: 'use ffiCall:options: instead'
		transformWith: '`@rec nbCall: `@arg1 options: `@arg2' -> '`@rec ffiCall: `@arg1 options: `@arg2'.
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: callOptions;
		function: fnSpec library: self ffiLibraryName
]

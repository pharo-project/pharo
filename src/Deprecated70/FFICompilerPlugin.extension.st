Extension { #name : #FFICompilerPlugin }

{ #category : #'*Deprecated70' }
FFICompilerPlugin class >> addAFfiCalloutSelectorEvent: aPragmaAdded [

	self
		deprecated: 'Please use #addFFICalloutSelectorEvent: instead'
		transformWith: '`@receiver addAFfiCalloutSelectorEvent: `@statements' -> '`@receiver addFFICalloutSelectorEvent: `@statements'.
		
	self addFFICalloutSelectorEvent: aPragmaAdded
]

{ #category : #'*Deprecated70' }
FFICompilerPlugin class >> addAFfiCalloutSelectorFromPragma: aPragma [

	self
		deprecated: 'Please use #addFFICalloutSelectorFromPragma: instead'
		transformWith: '`@receiver addAFfiCalloutSelectorFromPragma: `@statements' -> '`@receiver addFFICalloutSelectorFromPragma: `@statements'.
		
	self addFFICalloutSelectorFromPragma: aPragma
]

{ #category : #'*Deprecated70' }
FFICompilerPlugin class >> addFfiCalloutSelector: aString [

	self
		deprecated: 'Please use #addFFICalloutSelector: instead'
		transformWith: '`@receiver addFfiCalloutSelector: `@statements' -> '`@receiver addFFICalloutSelector: `@statements'.
		
	self addFFICalloutSelector: aString
]

{ #category : #'*Deprecated70' }
FFICompilerPlugin class >> defaultFfiCalloutSelectors [

	self
		deprecated: 'Please use #defaultFFICalloutSelectors instead'
		transformWith: '`@receiver defaultFfiCalloutSelectors' -> '`@receiver defaultFFICalloutSelectors'.
		
	^self defaultFFICalloutSelectors 
]

{ #category : #'*Deprecated70' }
FFICompilerPlugin class >> removeAFfiCalloutSelectorEvent: aPragmaRemoved [

	self
		deprecated: 'Please use #removeFFICalloutSelectorEvent: instead'
		transformWith: '`@receiver removeAFfiCalloutSelectorEvent: `@statements' -> '`@receiver removeFFICalloutSelectorEvent: `@statements'.
		
	self removeFFICalloutSelectorEvent: aPragmaRemoved 
]

{ #category : #'*Deprecated70' }
FFICompilerPlugin class >> removeFfiCalloutSelector: aString [

	self
		deprecated: 'Please use #removeFFICalloutSelector: instead'
		transformWith: '`@receiver removeFfiCalloutSelector: `@statements' -> '`@receiver removeFFICalloutSelector: `@statements'.
		
	self removeFFICalloutSelector: aString 
]

Extension { #name : 'CompiledMethod' }

{ #category : '*UnifiedFFI' }
CompiledMethod >> ffiArgumentNames [
	"Answer the method's argument names. We using a separate method,
	to get arg names not from source code directly, but from method properties, collected at
	compile time. Useful, when there is no source code available (for some reason)"

	^ self
		  propertyAt: #ffiArgumentNames
		  ifAbsent: [ self propertyAt: #ffiArgumentNames put: self argumentNames ]
]

{ #category : '*UnifiedFFI' }
CompiledMethod >> hasSelector: selector specialSelectorIndex: specialOrNil [
	"Answer whether this method refers to a selector. FFI methods require special
	consideration since a substitution of the original method is installed in the
	method dictionary."

	| ffiNonCompiledMethod |
	ffiNonCompiledMethod := self
		propertyAt: #ffiNonCompiledMethod
		ifAbsent: [
			^ super
				hasSelector: selector
				specialSelectorIndex: specialOrNil ].

	^ ffiNonCompiledMethod
		hasSelector: selector
		specialSelectorIndex: specialOrNil
]

{ #category : '*UnifiedFFI' }
CompiledMethod >> isFFIMethod [

	^ self hasProperty: #isFFIMethod
]

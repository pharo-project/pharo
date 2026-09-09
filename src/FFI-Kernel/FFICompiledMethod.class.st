Class {
	#name : 'FFICompiledMethod',
	#superclass : 'CompiledMethod',
	#type : 'compiledMethod',
	#category : 'FFI-Kernel',
	#package : 'FFI-Kernel'
}

{ #category : 'accessing - literals' }
FFICompiledMethod >> hasSelector: selector specialSelectorIndex: specialOrNil [
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

{ #category : 'testing' }
FFICompiledMethod >> isFFIMethod [

	^ true
]

Class {
	#name : #RGCompiledMethodLayout,
	#superclass : #RGObjectLayout,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'testing types' }
RGCompiledMethodLayout >> isBitsLayout [

	^ true
]

{ #category : #'testing types' }
RGCompiledMethodLayout >> isByteLayout [

	^ true
]

{ #category : #'testing types' }
RGCompiledMethodLayout >> isCompiledMethodLayout [

	^ true
]

{ #category : #'testing types' }
RGCompiledMethodLayout >> isVariableLayout [

	^ true
]

{ #category : #accessing }
RGCompiledMethodLayout >> layoutName [

	^ #CompiledMethodLayout
]

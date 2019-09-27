Class {
	#name : #FFIUndefinedTypeDeclaration,
	#superclass : #Object,
	#category : #'UnifiedFFI-Arguments'
}

{ #category : #testing }
FFIUndefinedTypeDeclaration >> isUndefined [

	^ true
]

{ #category : #resolution }
FFIUndefinedTypeDeclaration >> resolveUsing: aResolver forArgument: aFFIValueArgument [ 
	
	(aFFIValueArgument value = 'nil' or: [ aFFIValueArgument value = 'NULL' ])
		ifTrue: [ ^ (aResolver resolveType: #'void *') ].
	(#('true' 'false') includes: aFFIValueArgument value) 
		ifTrue: [ ^ aResolver resolveType: #bool ].
	aFFIValueArgument value isInteger
		ifTrue: [ ^ (aFFIValueArgument value >=0 
			ifTrue: [ aResolver resolveType: #uint32 ]
			ifFalse: [ aResolver resolveType: #int32 ]) ].
	(aFFIValueArgument value = 'self') ifTrue: [ 
		^ (aResolver requestor asExternalTypeOn: aResolver) 
			prepareAsSelfFromCalloutDeclaration ].
	self halt.
]

accessing
removeAtMethod: aCompiledMethod ifPresent: aBlock
	(examples 
		at: aCompiledMethod methodClass theNonMetaClass name 
		ifAbsent: [ ^ nil ]) 
			removeKey: aCompiledMethod selector 
			ifAbsent: [ ^ nil ].
	(examples at: aCompiledMethod methodClass theNonMetaClass name) 
		ifEmpty: [ examples removeKey: aCompiledMethod methodClass theNonMetaClass name ].
	aBlock value.
	^ aCompiledMethod
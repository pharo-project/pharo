private
flushCachesFor: aClass 
	selectorCache isNil ifTrue: [ ^ self] .
	aClass theNonMetaClass withAllSubclasses do: [ :each | 
		selectorCache
			removeKey: each name ifAbsent: [];
			removeKey: each theMetaClass name ifAbsent: [] ]
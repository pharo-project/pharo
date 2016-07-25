merging
mergeSpec: anotherSpec

	| newSpec map anotherProjectReference |
	newSpec := super mergeSpec: anotherSpec.
	map := anotherSpec mergeMap.
	(anotherProjectReference := map at: #projectReference) ~~ nil
		ifTrue: [ 
			newSpec projectReference: 
				(newSpec projectReference == nil
					ifTrue: [ anotherProjectReference ]
					ifFalse: [ newSpec projectReference mergeSpec: anotherProjectReference ])].
	^newSpec
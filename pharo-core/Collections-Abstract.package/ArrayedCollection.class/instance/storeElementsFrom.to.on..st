private
storeElementsFrom: firstIndex to: lastIndex on: aStream

	| noneYet defaultElement arrayElement |
	noneYet := true.
	defaultElement := self defaultElement.
	firstIndex to: lastIndex do: 
		[:index | 
		arrayElement := self at: index.
		arrayElement = defaultElement
			ifFalse: 
				[noneYet
					ifTrue: [noneYet := false]
					ifFalse: [aStream nextPut: $;].
				aStream nextPutAll: ' at: '.
				aStream store: index.
				aStream nextPutAll: ' put: '.
				aStream store: arrayElement]].
	^noneYet
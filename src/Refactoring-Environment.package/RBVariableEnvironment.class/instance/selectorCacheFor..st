private
selectorCacheFor: aClass 
	^self selectorCache at: aClass name ifAbsentPut: [ self computeSelectorCacheFor: aClass ]
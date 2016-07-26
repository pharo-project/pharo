private
selectorCache
	^selectorCache isNil 
		ifTrue: [selectorCache := Dictionary new]
		ifFalse: [selectorCache]
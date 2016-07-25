converting
repeat: aNumber

	"returns a new string concatenated by itself repeated n times
		'abc' repeat: 3. 'abcabcabc'
	"

	aNumber < 0 ifTrue: [ self error: 'aNumber cannot be negative' ].
	
	^ self class 
		new: self size * aNumber 
		streamContents: [ :stringStream |
			1 to: aNumber do: [ :idx | stringStream nextPutAll: self ].
		]
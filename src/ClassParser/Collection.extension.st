Extension { #name : #Collection }

{ #category : #'*ClassParser' }
Collection >> selectEvery: aBlock [
	"Select elements of the receiver whose index satisfy the argument. Look at selectEveryFirst and selectEverySecond for examples."
	
	| res |
	res := OrderedCollection new.
	1 to: self size do: [ :i | (aBlock value: i) 
											ifTrue: [res add: (self at: i) ]].
	^ res
]

{ #category : #'*ClassParser' }
Collection >> selectEveryFirst [
	"#(1 2 3 4) selectEveryFirst asArray 
	>>> #(1 3)
	"
	^ self selectEvery: [ :i | (i+1 \\ 2) = 0 ]
]

{ #category : #'*ClassParser' }
Collection >> selectEverySecond [
	"#(1 2 3 4) selectEverySecond asArray 
	>>> #(2 4)
	"
	^ self selectEvery: [ :i | i \\ 2 = 0 ]
]

{ #category : #'*ClassParser' }
Collection >> unzip [
	"Given a collection returns in one pass two collections containing each odd and even elements in respective subcollections"
	"
	#(1 2 3 4) unzip first asArray 
	>>> #(1 3)
	#(1 2 3 4) unzip second asArray 
	>>> #(2 4)
	"
	
	| odd even s |
	s := self size // 2.
	odd := OrderedCollection new: s.
	even := OrderedCollection new: s.
	1 
		to: self size 
		do: [ :i | (i \\ 2) = 0	
						ifTrue: [ even add: (self at: i) ]
						ifFalse: [ odd add: (self at: i) ] ].
	^ { odd . even }
]

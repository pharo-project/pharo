enumerating
flatCollect: aBlock 
	"Evaluate aBlock for each of the receiver's elements and answer the
	list of all resulting values flatten one level. Assumes that aBlock returns some kind
	of collection for each element. optimized version for Sequencable Collection and subclasses
	implementing #writeStream"
	"
	#( (2 -3) (4 -5) #(-6)) flatCollect: [ :e | e abs  ]  
	>>> #(2 3 4 5 6)
	"
	"
	#( (2 -3) #((4 -5)) #(-6)) flatCollect: [ :e | e abs  ]  
	>>> #(2 3 #(4 5) 6)
	"
	
	| stream |
	self isEmpty ifTrue: [ ^ self copy ].
	stream := (self species new: 0) writeStream.
	self do: [ :each | stream nextPutAll: (aBlock value: each) ].
	^ stream contents
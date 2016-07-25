private
string: aString linesDo: aBlock

	| stream cr current|
	
	stream := aString readStream.
	current := String new writeStream.
	cr := Character cr.
	
	[ stream atEnd ] whileFalse: [ | c |
		(c := stream next) == cr ifTrue: [
			aBlock value: current contents.
			current := String new writeStream ]
		ifFalse: [ current nextPut: c ]
	].
	aBlock value: current contents
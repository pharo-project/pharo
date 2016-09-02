printing
print: anObject
	
	^String streamContents: [ :s |	self print: anObject on: s] limitedTo: 32.

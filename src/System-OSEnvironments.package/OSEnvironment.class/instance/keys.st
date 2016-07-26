accessing
keys
	"Answer an Array containing the receiver's keys."
	
	^Array streamContents: [ :s | self keysDo: [ :key | s nextPut: key]]
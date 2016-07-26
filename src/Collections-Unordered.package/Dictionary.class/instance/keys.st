accessing
keys
	"Answer an Array containing the receiver's keys."
	
	^Array new: self size streamContents: [:s| self keysDo: [:key| s nextPut: key]]
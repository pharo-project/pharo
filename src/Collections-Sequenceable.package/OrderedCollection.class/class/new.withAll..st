instance creation
new: anInteger withAll: anObject
	^ self basicNew setContents: (self arrayType new: anInteger withAll: anObject)
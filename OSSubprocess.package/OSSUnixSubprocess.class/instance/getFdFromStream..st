close and dup2
getFdFromStream: aStream
	^ self systemAccessor fileno: aStream getFilePointerAsCFile
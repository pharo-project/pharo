*OSSubprocess
atEndOfFile
	"Answer whether the receiver is at its end based on the result of the last read operation. This uses feof() to test the underlying file stream status, and can be used as an alternative to #atEnd, which does not properly report end of file status for an OSPipe.
	
	Answer whether the file represented by #getFilePointerAsCFile is at end of file, as determined by a call to feof(). This is different from StandardFileStream>>primAtEnd: which answers true if the file pointer is at the end of the file, but which does not call feof() to determine that an end of file condition has occurred. The difference is significant	if aSqFileStruct represents a pipe. Another case where the difference is significant is when using StandardFileStream to communicate with a child process since  the SQFile will not automatically be updated if some other process has changed the actual size of the file.
	"

	^ fileID isNil or: [ self systemAccessor feof: self getFilePointerAsCFile ]

TO USE LATER WITHOUT OSPROCESS
oldname: aSymbolOrString attachTo: externalFilePointer writable: readWriteFlag
	"Attach to an existing file handle, assumed to have been previously opened by the underlying operating system.
	We assume a 32 bits machine and here we document the SQFile used by the VM:
	/* squeak file record; see sqFilePrims.c for details */
typedef struct {
  int			 sessionID;	/* ikp: must be first */
  void			*file;
  squeakFileOffsetType	 fileSize;	/* 64-bits we hope. */
  char			 writable;
  char			 lastOp; /* 0 = uncommitted, 1 = read, 2 = write */
  char			 lastChar;
  char			 isStdioStream;
} SQFile;
	"
	| fileIDByteArray |
	"Hack...I calculated that the size of the SQFile struct in a 32 bits machine is 20 bytes. "
	fileIDByteArray := ByteArray new: 20. 
	name := aSymbolOrString.
	self setSessionIDTo: fileIDByteArray.
	"Hack. I calculated that the File* of the SQFile is the second element of the struct and starts at the byte 5.
	The first 4 byets is the session ID. "
	self setOSFilePointerFromByteArray: externalFilePointer getHandle getHandle asByteArrayPointer to: fileIDByteArray offset: 4.
	readWriteFlag 
		ifTrue: [
			"Again, in 32 bytes machines, the byte17 is the variable 'writable' from the SQFile.  "
			fileIDByteArray at: 17 put: 1.
			self readWrite] 
		ifFalse: [
			self readOnly].
	fileID := fileIDByteArray.
	self ascii.
	self register

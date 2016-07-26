TO USE LATER WITHOUT OSPROCESS
name: aSymbolOrString attachToCFile: externalFilePointer writable: readWriteFlag
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
	self flag: #fixIt.
	"Hack...I calculated that the size of the SQFile struct in a 32 bits machine is 20 bytes only
	if the fileSize was 64 bits. This is the case of Pharo OSX VM. However, Linux VM has not yet been build
	with 64 buts but 32 bits for fileSize and so, instead of 20 it is 16.
	For more details read thread with subject 'Why StandardFileStream fileID is 16 bytes in Linux while 20 bytes in OSX?'
	"
	fileIDByteArray := ByteArray new: 20. 
	name := aSymbolOrString.
	self setSessionIDTo: fileIDByteArray.
	"Hack. I calculated that the File* of the SQFile is the second element of the struct and starts at the byte 5.
	The first 4 byets is the session ID. "
	self setOSFilePointerFromByteArray: externalFilePointer getHandle getHandle asByteArrayPointer to: fileIDByteArray offset: 4.
	readWriteFlag 
		ifTrue: [
			self flag: #fixIt.
			"Again, in 32 bytes machines, the byte17 is the variable 'writable' from the SQFile. 
			In Linux, instead if 17 it should be 13.
			 "
			fileIDByteArray at: 17 put: 1.
			self readWrite] 
		ifFalse: [
			self readOnly].
	fileID := fileIDByteArray.
	self ascii.
	self register

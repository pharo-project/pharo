close and dup2
allocatePosixSpawnFilesActionStructure
	"IMPORTANT:  It seems posix_spawn_file_actions_init() sometimes allocates AND initializes, and sometimes only initializes.
	For OSX I found it it also allocates memory, so only allocating for the pointer is enough.
	For Linux it seems we need to allocate the full structure of posix_spawn_file_actions_t.
	Wathing at the implementation of GNU libc (glibc) here: http://osxr.org:8080/glibc/source/posix/spawn.h?v=glibc-2.17 
	I found this struct definition:
	
	typedef struct
	 {
	   int __allocated;
	   int __used;
	   struct __spawn_action *__actions;
	   int __pad[16];
	 } posix_spawn_file_actions_t;
	
	We need a better solution to solve this kind of sizeof(). For the moment, see below how I estimate the size of this struct.  
	For more details see thread 'Help with FFI crash in latest Spur (only in Linux)'
	"
	OSPlatform current isMacOSX ifTrue: [ posixSpawnFileActionsT := ExternalAddress allocate: self systemAccessor sizeOfPointer. ].
	OSPlatform current isUnix ifTrue: [ posixSpawnFileActionsT := ExternalAddress allocate: (self systemAccessor sizeOfPointer + (self systemAccessor sizeOfInt * 2) + (16 * self systemAccessor sizeOfInt)). ].
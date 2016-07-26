pipes
makePipeWithReadBlocking: aBoolean
	"Originally, we created the pipes ourselves and the answer was an array of file descriptors.
	This was complicated because we have to then create our own sqFile structure and deal
	with with offsets and their sizeof(). 
	
	So now we use OSProcess primitive primCreatePipe which directly answers an array of sqFile structs
	specifially created for them. This way we avoid all the sqFile creation and management at image side. 	
	"

" ==================== OLD CODe =======================
	| pipePointer returnValue fileDescriptors |
	pipePointer := ExternalAddress allocate: 8.
	[
		returnValue := self primitivePipe: pipePointer.	
		(returnValue = -1) ifTrue: [ self perror: 'pipe()' ]. 
		fileDescriptors := Array new: 2.
		fileDescriptors at: 1 put: (pipePointer nbUInt32AtOffset: 0).
		fileDescriptors at: 2 put: (pipePointer nbUInt32AtOffset: 4).
		^ OSSPipe newWith: fileDescriptors readBlocking: aBoolean. 
	] ensure:[
		pipePointer free.
	]
 ==================== OLD CODe =======================
"

	| fileIDsArray |
	fileIDsArray := self primCreatePipe.
	^ OSSPipe newWith: fileIDsArray readBlocking: aBoolean. 

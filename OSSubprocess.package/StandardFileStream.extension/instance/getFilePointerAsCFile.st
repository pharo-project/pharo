*OSSubprocess
getFilePointerAsCFile

	^ OSSCFile new 
		setHandle: (ExternalData 
							fromHandle: ((fileID copyFrom: 5 to: 8) asExternalPointer  ) 
							type: ExternalType void asPointerType);
		yourself
	



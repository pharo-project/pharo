instance creation
fromPointer: aHandle type: aType size: aNumber
	"Creates an array of type aType and size aNumber.
	 aHandle is A REFERENCE an array already existing. 
	 For example, C: 
	 	
		byte[]	 array;
		void * aHandle = &array;
			
	 Which means first element of array will be found at ==aHandle pointerAt: 1=="
	^ self fromHandle: (aHandle pointerAt: 1) type: aType size: aNumber
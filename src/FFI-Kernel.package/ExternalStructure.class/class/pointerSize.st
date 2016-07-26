accessing
pointerSize
	"Answer the size of pointers for this class.  By default answer nil.
	 Subclasses that contain pointers must define the size of a pointer if the code is to operate on 64-bit and 32-bit platforms.
	 Currently we have no way of converting a type between 32- and 64- bit versions beyond recompiling it."
	^nil
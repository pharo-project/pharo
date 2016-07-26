private
initializeHighLights 
	
	"Create a set of Bitmaps for quickly reversing areas of the screen without converting colors. "
	
	HighLightBitmaps := Array new: 32.
	^HighLightBitmaps
		at: 1 put: (Bitmap with: 4294967295);
		at: 2 put: (Bitmap with: 4294967295);
		at: 4 put: (Bitmap with: 1431655765);
		at: 8 put: (Bitmap with: 117901063);
		at: 16 put: (Bitmap with: 4294967295);
		at: 32 put: (Bitmap with: 4294967295);
		yourself.

"
I'm a module defining access to standard LibC. 
I'm available under Linux and OSX, but not under Windows for obvious reasons :)
"
Class {
	#name : #LibC,
	#superclass : #FFILibrary,
	#category : #'UnifiedFFI-Libraries'
}

{ #category : #misc }
LibC class >> memCopy: from to: to size: size [
	^ self uniqueInstance  
		memCopy: from 
		to: to 
		size: size
]

{ #category : #misc }
LibC class >> system: command [
	^ self uniqueInstance system: command
]

{ #category : #'accessing platform' }
LibC >> macModuleName [
	^ 'libc.dylib'
]

{ #category : #misc }
LibC >> memCopy: src to: dest size: n [
	^ self ffiCall: #(void *memcpy(void *dest, const void *src, size_t n))
]

{ #category : #misc }
LibC >> system: command [
	^ self ffiCall: #(int system #(char * command))
]

{ #category : #'accessing platform' }
LibC >> unixModuleName [
	^ 'libc.so.6'
]

{ #category : #'accessing platform' }
LibC >> win32ModuleName [
	"While this is not a 'libc' properly, msvcrt has the functions we are defining here"
	^ 'msvcrt.dll'
]

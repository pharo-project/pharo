"
This is a wrapper for the global 'library' in the plugin.
It is provided for the use of Cairo APIs that take an FT_Library argument.
"
Class {
	#name : #FT2Library,
	#superclass : #FT2Handle,
	#classVars : [
		'Current'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Base'
}

{ #category : #'instance creation' }
FT2Library class >> current [
	^ Current ifNil: [ Current := self basicNew ].
]

{ #category : #private }
FT2Library >> checkLibrary [
	| errorCode holder |
	"I have a valid address"
	self isNull ifFalse: [ ^ self ].

	"I try to get a valid address if I do not have one"
	holder := PointerHolder new.
	errorCode := self ffiInitFreeType: holder.
	errorCode ~= 0
		ifTrue: [ FT2Error errorCode: errorCode errorString: (FT2FFILibrary translateErrorCode: errorCode) signal: 'Error initializing FreeType Library: ' ].
	self setHandle: holder value
]

{ #category : #'private-ffi-calls' }
FT2Library >> ffiDoneFace: aHandle [

	self ffiCall: #(int FT_Done_Face(void* aHandle))
]

{ #category : #'private-ffi' }
FT2Library >> ffiGetBitmap: abitmap fromOutline: outline [
	self
		ffiCall:
			#(FT_Error FT_Outline_Get_Bitmap (self, void* outline , void* abitmap))
]

{ #category : #'ffi-calls' }
FT2Library >> ffiInitFreeType: aPointer [

	self ffiCall: #(int FT_Init_FreeType(PointerHolder* aPointer))
]

{ #category : #'private-ffi-calls' }
FT2Library >> ffiLibraryVersionMajor: major minor: minor patch: patch [
	self ffiCall: #(void FT_Library_Version #(self , IntHolder * major , IntHolder * minor , IntHolder* patch))
]

{ #category : #'private-ffi-calls' }
FT2Library >> ffiNewFace: aface fromFilename: filepathname index: face_index [

	self ffiCall: #(int FT_New_Face(self,
               String  filepathname,
               FT_Long face_index,
               PointerHolder *aface ))
]

{ #category : #'private-ffi-calls' }
FT2Library >> ffiNewFace: aface fromMemory: file_base size: file_size index: face_index [

self ffiCall: #(int FT_New_Memory_Face(self,
                      void*  			  file_base,
                      long         file_size,
                      long         face_index,
                      PointerHolder   *aface ))
]

{ #category : #'private-ffi-calls' }
FT2Library >> ffiOutline: outline getBitmap: abitmap [
	self
		ffiCall: #(int FT_Outline_Get_Bitmap #(self , FT_Outline * outline , FT_Bitmap * abitmap))
]

{ #category : #reading }
FT2Library >> getBitmap: ftBitmap fromOutline: outline [ 
	|returnCode|
	self checkLibrary.
	returnCode := self ffiGetBitmap: ftBitmap fromOutline: outline.
	returnCode ~= 0 ifTrue: [ FT2Error errorCode: returnCode signal: 'Error reading bitmap from outline' ].
]

{ #category : #testing }
FT2Library >> isAvailable [
	[self checkLibrary] on:Error do: [^ false].
	^ self isNull not
]

{ #category : #version }
FT2Library >> libraryVersion [
	| version major minor patch|
	major := IntHolder new.
	minor := IntHolder new.
	patch := IntHolder new.
	
	self checkLibrary.
	self ffiLibraryVersionMajor:major minor: minor patch: patch. 
	
	version := FT2Version new.
	version major: major value.
	version minor: minor value.
	version patch: patch value.
	^ version.
]

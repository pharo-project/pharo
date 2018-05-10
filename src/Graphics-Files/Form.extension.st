Extension { #name : #Form }

{ #category : #'*Graphics-Files-FileRegistry' }
Form class >> fileReaderServicesForFile: fullName suffix: suffix [
	<fileService>

	^((ImageReadWriter allTypicalFileExtensions add: '*'; add: 'form'; yourself)
		includes: suffix)
		ifTrue: [ self services ]
		ifFalse: [#()]

]

{ #category : #'*Graphics-Files' }
Form class >> fromBinaryStream: aBinaryStream [
	"Read a Form or ColorForm from given file, using the first byte of the file to guess its format. Currently handles: GIF, uncompressed BMP, and both old and new DisplayObject writeOn: formats, JPEG, and PCX. Return nil if the file could not be read or was of an unrecognized format."

	| firstByte |
	firstByte := aBinaryStream peek.
	firstByte = 1 ifTrue: [
		"old Squeakform format"
		^ self new readFromOldFormat: aBinaryStream].
	firstByte = 2 ifTrue: [
		"new Squeak form format"
		^ self new readFrom: aBinaryStream].

	"Try for JPG, GIF, or PCX..."
	"Note: The following call closes the stream."
	^ ImageReadWriter formFromStream: aBinaryStream

]

{ #category : #'*Graphics-Files' }
Form class >> fromFileNamed: fileName [ 
	"Read a Form or ColorForm from the given file."
	
	^ fileName asFileReference 
			binaryReadStreamDo: [ :in | self fromBinaryStream: in ]
]

{ #category : #'*Graphics-Files-FileRegistry' }
Form class >> openImageInWindow: fullName [ 
	"Handle five file formats: GIF, JPG, PNG, Form storeOn: (run coded), and
	BMP. Fail if file format is not recognized."
	
	| image |
	image := self fromFileNamed: fullName.
	(World drawingClass withForm: image) openInWorld
]

{ #category : #'*Graphics-Files' }
Form >> readNativeResourceFrom: byteStream [
	| img |
	img := [ ImageReadWriter formFromStream: byteStream ]
		on: Error
		do: [ :ex | ^ nil ].
	(img isColorForm and: [ self isColorForm ])
		ifTrue: [ | cc |
			cc := img colors.
			img colors: nil.
			img displayOn: self.
			img colors: cc ]
		ifFalse: [ img displayOn: self ]
]

{ #category : #'*Graphics-Files-FileRegistry' }
Form class >> serviceImageAsBackground [
	"Answer a service for setting the desktop background from a given graphical file's contents"

	^ SimpleServiceEntry 
		provider: self 
		label: 'Use graphic as background'
		selector: #setBackgroundFromImageFileNamed:
		description: 'Use the graphic as the background for the desktop'
		buttonLabel: 'Background'
]

{ #category : #'*Graphics-Files-FileRegistry' }
Form class >> serviceOpenImageInWindow [
	"Answer a service for opening a graphic in a window"

	^ SimpleServiceEntry 
		provider: self 
		label: 'Open graphic in a window'
		selector: #openImageInWindow:
		description: 'Open a graphic file in a window'
		buttonLabel: 'Open'
]

{ #category : #'*Graphics-Files-FileRegistry' }
Form class >> services [

	^ Array 
		with: self serviceOpenImageInWindow
		with: self serviceImageAsBackground 
]

{ #category : #'*Graphics-Files-FileRegistry' }
Form class >> setBackgroundFromImageFileNamed: aFileName [

	(self fromFileNamed: aFileName) setAsBackground
]

{ #category : #'*Graphics-Files' }
Form >> writeBMPfileNamed: fName [  "Display writeBMPfileNamed: 'display.bmp'"
	BMPReadWriter putForm: self onFileNamed: fName
]

{ #category : #'*Graphics-Files' }
Form >> writeJPEGfileNamed: fileName [ 
	"Write a JPEG file to the given filename using default settings"

	self writeJPEGfileNamed: fileName progressive: false

"
Display writeJPEGfileNamed: 'display.jpeg'
Form fromUser writeJPEGfileNamed: 'yourPatch.jpeg'
"
]

{ #category : #'*Graphics-Files' }
Form >> writeJPEGfileNamed: fileName  progressive: aBoolean [
	"Write a JPEG file to the given filename using default settings.  Make it progressive or not, depending on the boolean argument"

	PluginBasedJPEGReadWriter putForm: self quality: -1 "default" progressiveJPEG: aBoolean onFileNamed: fileName

"
Display writeJPEGfileNamed: 'display.jpeg' progressive: false.
Form fromUser writeJPEGfileNamed: 'yourPatch.jpeg' progressive: true
"
]

{ #category : #'*Graphics-Files' }
Form >> writeOnMovie: file [
	"Write just my bits on the file."
	self unhibernate.
	bits writeUncompressedOn: file
]

{ #category : #'*Graphics-Files' }
Form >> writePNGFileNamed: fileName [  
	PNGReadWriter putForm: self onFileNamed: fileName
]

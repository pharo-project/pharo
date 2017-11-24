"
A help topic on how to import resources into the image
"
Class {
	#name : #ImportingResourceHelp,
	#superclass : #CustomHelp,
	#category : #'ImportingResource-Help'
}

{ #category : #accessing }
ImportingResourceHelp class >> bookName [ 
	^'Importing Resources'
]

{ #category : #documentation }
ImportingResourceHelp class >> encodingAndDecoding [
	^ HelpTopic 
		title: 'Encoding and Decoding'
		contents: (self heading: 'Encoding and Decoding'),
'The basic mechanism to encode and decode data is provided by the class Base64MimeConverter. Consider the following example:

mimeEncoded := ''Hello World'' asByteArray base64Encoded.
(Base64MimeConverter mimeDecodeToBytes: mimeEncoded readStream) contents asString

The variable mimeEncoded contains an encoding of the Hello World string using the mime64 encoding. 
The method #mimeDecodeToBytes: reads mime64 strings from a stream. The example uses a string, but it could refers to any objects. You essentially need a ReadStream.
 '
]

{ #category : #documentation }
ImportingResourceHelp class >> encodingAndDecodingImage [
	^ HelpTopic 
		title: 'Encoding and Decoding Image'
		contents: (self heading: 'Encoding and Decoding Image'),
'Embedding pictures in plan source code is often the reason why one need to encode resources. The following script loads an image located in the filesystem, create a string-friendly representation, and store it in a method. 

You probably want to keep the image in a class variable or a hash table.

',
(self bold: '
form := PNGReadWriter formFromStream: ''/Users/alexandrebergel/Dropbox/Screenshots/Screenshot 2017-05-19 15.25.44.png'' asFileReference readStream.

"Write the stream on a stream"
w := WriteStream on: ByteArray new.
PNGReadWriter putForm: form onStream: w.
w contents.


"Convert the byteArray into mime 64. It takes less space in the image"
encodedContent := w contents base64Encoded.

"The following code converts the stream content "
PNGReadWriter formFromStream: (Base64MimeConverter mimeDecodeToBytes: encodedContent readStream).


"We encode the image as a method"
Object compile: ''imageSource  ^ '''',  encodedContent, ''''.
Object compile: ''image ^ PNGReadWriter formFromStream: (Base64MimeConverter mimeDecodeToBytes: self imageSource readStream). ''.

"A better implementation is to cache the image in an instance or class variable"

"The following expression return the image initially encoded"
Object new image 
'),'
'
]

{ #category : #documentation }
ImportingResourceHelp class >> importingIcon [
	^ HelpTopic 
		title: 'Importing Icon'
		contents: 
'The easiest way to create icons from a bunch of .png pictures is to use IconFactory. 

1 - You can load it from the Catalog Browser, available from the WorldMenu/Tools.

2 - Once loaded, you need to create an empty class, which will contains your icons. Create a subclass of Object. For the purpose of the example, we will call it MatrixCubeIcons 

3 - You need to prepare the icon class to handle icons. For example, to provide caches. Simply evaluate:
	IconFactory setup: MatrixCubeIcons
	
4 - The last step is to import the .png files in the icon class:
	IconFactory createIconsFromDirectory: ''/Users/alexandrebergel/Desktop/Cube'' inClass: MatrixCubeIcons

After this last step, the class MatrixCubeIcons will contains the icons found in the directory /Users/alexandrebergel/Desktop/Cube
The icon class should be packaged within your application. 

Note that icons classes are GT-compatible. Simply inspect the expression: MatrixCubeIcons current

 '
]

{ #category : #documentation }
ImportingResourceHelp class >> overview [
	^ HelpTopic 
		title: 'Overview'
		contents: (self heading: 'Overview'),
'Non trivial applications often rely on resources that may be externally provided such as icons, pictures, sounds. Such resources are typically defined as external files. Naturally, you can make your application depends on external files, however, importing resource files within the Pharo image and make it integrally part of you code has a number of advantages:
- you can simply deploy your application by downloading it from smalltalkhub. 
- having less dependencies on external files is always good to avoid technical subtilities such as file path containing weird characters.
 '
]

{ #category : #accessing }
ImportingResourceHelp class >> pages [
	^#(overview encodingAndDecoding encodingAndDecodingImage)
]

documentation
encodingAndDecodingImage
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
documentation
encodingAndDecoding
	^ HelpTopic 
		title: 'Encoding and Decoding'
		contents: (self heading: 'Encoding and Decoding'),
'The basic mechanism to encode and decode data is provided by the class Base64MimeConverter. Consider the following example:

mimeEncoded := ''Hello World'' asByteArray base64Encoded.
(Base64MimeConverter mimeDecodeToBytes: mimeEncoded readStream) contents asString

The variable mimeEncoded contains an encoding of the Hello World string using the mime64 encoding. 
The method #mimeDecodeToBytes: reads mime64 strings from a stream. The example uses a string, but it could refers to any objects. You essentially need a ReadStream.
 '
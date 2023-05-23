# Importing Resources

## Overview

Non trivial applications often rely on resources that may be externally provided such as icons, pictures, sounds. Such resources are typically defined as external files. Naturally, you can make your application depends on external files, however, importing resource files within the Pharo image and make it integrally part of you code has a number of advantages:

- you can simply deploy your application by downloading it from git.
- having less dependencies on external files is always good to avoid technical subtleties such as file path containing weird characters.

## Encoding and Decoding

The basic mechanism to encode and decode data is provided by the class `Base64MimeConverter`. 

The key point to understand is that we need to encode binary on string (that we want to store in method body). So we have to change the representation going back and forth between binary and string.

Consider the following example:

```st
mimeEncoded := 'Hello World' asByteArray base64Encoded.
(Base64MimeConverter mimeDecodeToBytes: mimeEncoded readStream) contents asString
```

The variable `mimeEncoded` contains an encoding of the Hello World string using the mime64 encoding. 
The method `mimeDecodeToBytes:` reads mime64 strings from a stream. The example uses a string, but it could refers to any objects. You essentially need a `ReadStream`.

## Encoding and Decoding Image

Embedding pictures in plain source code is often the reason why one need to encode resources. The following script loads an image located in the filesystem, create a string-friendly representation, and store it in a method. 

#### Reading a PNG

You probably want to keep the image in a variable or a hash table.

```st
form := PNGReadWriter formFromStream: '/Users/alexandrebergel/Dropbox/Screenshots/Screenshot 2017-05-19 15.25.44.png' asFileReference readStream.
```

#### Write the stream on a stream

```st
w := WriteStream on: ByteArray new.
PNGReadWriter putForm: form onStream: w.
w contents.
```

#### Convert the byteArray into mime 64

It takes less space in the image

```st
encodedContent := w contents base64Encoded.
```

The following code converts the stream content.

```st
PNGReadWriter formFromStream: (Base64MimeConverter mimeDecodeToBytes: encodedContent readStream).
```

#### Encode the image as a method

Here we compile two methods: `imageSource` which returns 
an encoded image and `image` which returns the PNG from the encoded string. 

```st
Object compile: 'imageSource  ^ '',  encodedContent, ''.
Object compile: 'image ^ PNGReadWriter formFromStream: (Base64MimeConverter mimeDecodeToBytes: self imageSource readStream)'
```

A better implementation is to cache the image in an instance or class variable

The following expression returns the image initially encoded.

```st
Object new image 
```

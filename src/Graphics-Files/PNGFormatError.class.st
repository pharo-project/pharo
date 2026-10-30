"
I am the error signalled by `PNGReadWriter` when the bytes it decodes break the PNG specification.

## Responsibility:

- I report a malformed PNG file: a missing or invalid IHDR chunk, a forbidden combination of color type and bit depth, an unknown compression, filter or interlace method, or an invalid filter type on a scanline.
- My message text says which rule of the specification is broken.
- I let callers tell a corrupt PNG apart from a bug in the decoder or from an unrecognized image format (`UnrecognizedImageFormatError`).

## Collaborators: 

- `PNGReadWriter` signals me while reading the header and the image data.
- `Error` is my superclass: I can be handled like any other error.

## Public API and Key Messages

- `PNGFormatError signal: 'a description'` to report a problem.
- `messageText` to know which rule is broken.

Example, decoding bytes that may not be a valid PNG:

```
[ PNGReadWriter formFromStream: someBytes readStream ]
	on: PNGFormatError
	do: [ :error | self inform: 'Corrupt PNG: ' , error messageText ]
```
"
Class {
	#name : 'PNGFormatError',
	#superclass : 'Error',
	#category : 'Graphics-Files',
	#package : 'Graphics-Files'
}

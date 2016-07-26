Copyright (c) Kazuki Yasumatsu, 1995. All rights reserved.

I am an abstract class to provide for encoding and/or decoding an image on a stream.

Instance Variables:
	stream		<ReadStream | WriteStream>	stream for image storages

Class Variables:
	ImageNotStoredSignal		<Signal>	image not stored error signal
	MagicNumberErrorSignal		<Signal>	magic number error signal

Subclasses must implement the following messages:
	accessing
		nextImage
		nextPutImage:
	testing
		canUnderstand         (added tao 10/26/97)
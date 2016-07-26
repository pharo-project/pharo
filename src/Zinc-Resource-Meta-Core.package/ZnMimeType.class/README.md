I am ZnMimeType.
Mime types are an element used in describing resources, more specifically their format.

Here are some examples MIME types:

	text/plain
	text/html
	text/plain;charset=utf-8
	image/png
	text/*
 
A mime type has a main/sub form with optional parameters. 

For more information: http://en.wikipedia.org/wiki/MIME_type
I know whether I am binary or not and know my charset when applicable.
My class side offers access to some common constants.
Note that for text types we default to UTF-8 encoding.

Examples:

	ZnMimeType textPlain = 'text/plain;charset=utf-8' asZnMimeType.
	ZnMimeType textPlain charSet.
	ZnMimeType textHtml matches: ZnMimeType text.
	ZnMimeType default matches: ZnMimeType any.
	ZnMimeType imagePng isBinary.
	ZnMimeType forFilenameExtension: 'html'.

I started life as a copy of WAMineType.

Part of Zinc HTTP Components.
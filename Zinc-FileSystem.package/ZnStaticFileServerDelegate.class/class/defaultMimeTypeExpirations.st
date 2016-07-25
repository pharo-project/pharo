accessing
defaultMimeTypeExpirations
	"Return a default dictionary mapping ZnMimeTypes to their expiration time in seconds.
	Missing entries will have no expiration time."

	| dict |
	dict := Dictionary new.
	dict at: ZnMimeType textCss put: 30 days asSeconds.
	dict at: ZnMimeType textJavascript put: 30 days asSeconds.
	dict at: ZnMimeType textPlain put: 30 days asSeconds.
	dict at: ZnMimeType textHtml put: 30 days asSeconds.
	dict at: ZnMimeType imageGif put: 30 days asSeconds.
	dict at: ZnMimeType imageJpeg put: 30 days asSeconds.
	dict at: ZnMimeType imagePng put: 30 days asSeconds.
	dict at: ZnMimeType applicationJavascript put: 30 days asSeconds.
	^ dict
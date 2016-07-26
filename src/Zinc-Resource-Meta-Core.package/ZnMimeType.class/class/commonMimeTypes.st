accessing
commonMimeTypes
	"Return a list of commom mime types, the ones we provided convenience accessors for."

	^ #( 
			applicationFormUrlEncoded applicationJavascript applicationJson 
			applicationOctetStream applicationPdf applicationXml 
			imageGif imageJpeg imagePng imageSvg 
			textCss textHtml textJavascript textPlain 
		) collect: [ :each | self perform: each ]
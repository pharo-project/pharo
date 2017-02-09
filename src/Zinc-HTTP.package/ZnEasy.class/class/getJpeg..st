operations
getJpeg: urlObject
	"self getJpeg: 'http://stfx.eu/sun-fire-x2100.jpg'."
	
	^ self 
		getImageOfType: ZnMimeType imageJpeg 
		fromUrl: urlObject
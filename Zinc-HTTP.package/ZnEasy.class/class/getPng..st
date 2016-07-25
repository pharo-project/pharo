operations
getPng: urlObject
	"self getPng: 'http://pharo.org/files/pharo.png'."
	"(self getPng: 'http://chart.googleapis.com/chart?cht=tx&chl=',
		'a^2+b^2=c^2' urlEncoded) asMorph openInHand."
	
	^ self 
		getImageOfType: ZnMimeType imagePng 
		usingParser: PNGReadWriter 
		fromUrl: urlObject 
	
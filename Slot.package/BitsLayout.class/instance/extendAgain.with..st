reshaping
extendAgain: aLayout with: aScope 
	"Answer my default layout since any subclass of me only can have my layout type."

	^ self extend
		host: host;
		yourself
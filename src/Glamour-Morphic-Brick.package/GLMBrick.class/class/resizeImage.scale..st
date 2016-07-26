utils
resizeImage: anImage scale: aScale

	((aScale > (1@0)) | (aScale > (0@1))) ifTrue: [
		^ self increaseFormExtentCentered: anImage to: (anImage extent * aScale).
	].

	aScale = (1@1) ifTrue: [
		^ anImage
	].
	
	^ anImage magnify: anImage boundingBox by: aScale smoothing: 2.
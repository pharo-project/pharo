drawing-general
fullDrawMorph: aMorph
	"Hook method for potential other canvases. In the core, this method looks supefluous but PostscriptCanvases and other canvases can specialized it for Morph rendering. Therefore it should not be merged with fullDraw:."

	self fullDraw: aMorph
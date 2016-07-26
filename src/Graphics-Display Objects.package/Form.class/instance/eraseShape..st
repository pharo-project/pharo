filling
eraseShape: bwForm
	"use bwForm as a mask to clear all pixels where bwForm has 1's"
	((BitBlt  destForm: self sourceForm: bwForm 
		fillColor: nil
		combinationRule: Form erase1bitShape	"Cut a hole in the picture with my mask"
		destOrigin: bwForm offset 
		sourceOrigin: 0@0
		extent: self extent clipRect: self boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF))
		copyBits.

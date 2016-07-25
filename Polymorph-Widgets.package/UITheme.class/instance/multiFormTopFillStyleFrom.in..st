fill-styles
multiFormTopFillStyleFrom: anArray in: aRectangle
	"Answer a composite fill style from the given forms and central colour
	in the given bounds. Only top and middle sections are considered."
	
	|tl tm tr ml mr c|
	tl := anArray first.
	tm := anArray second.
	tr := anArray third.
	ml  := anArray fourth.
	c := anArray fifth.
	mr  := anArray sixth.
	^CompositeFillStyle fillStyles: {
		(ImageFillStyle form: tl) origin: aRectangle topLeft.
		(AlphaInfiniteForm with: tm)
			origin: aRectangle topLeft + (tl width@0);
			extent: (aRectangle width - tl width - tr width)@tm height.
		(ImageFillStyle form: tr)
			origin: aRectangle topRight - (tr width@0).
		(AlphaInfiniteForm with: ml)
			origin: aRectangle topLeft + (0@tr height);
			extent: ml width@(aRectangle height - tl height).
		(ColorFillStyle color: c)
			origin: aRectangle topLeft + tl extent;
			extent: aRectangle extent - tl extent - (mr width @0).
		(AlphaInfiniteForm with: mr)
			origin: aRectangle topRight + (tr width negated@tr height);
			extent: mr width@(aRectangle height - tr height)}
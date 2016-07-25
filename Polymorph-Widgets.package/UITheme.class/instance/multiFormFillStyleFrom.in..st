fill-styles
multiFormFillStyleFrom: anArray in: aRectangle
	"Answer a composite fill style from the given forms and central colour
	in the given bounds."
	
	|tl tm tr ml mr bl bm br c|
	tl := anArray first.
	tm := anArray second.
	tr := anArray third.
	ml  := anArray fourth.
	c := anArray fifth.
	mr  := anArray sixth.
	bl := anArray seventh.
	bm := anArray eighth.
	br := anArray ninth.
	^CompositeFillStyle fillStyles: {
		(ImageFillStyle form: tl) origin: aRectangle topLeft.
		(AlphaInfiniteForm with: tm)
			origin: aRectangle topLeft + (tl width@0);
			extent: (aRectangle width - tl width - tr width)@tm height.
		(ImageFillStyle form: tr)
			origin: aRectangle topRight - (tr width@0).
		(AlphaInfiniteForm with: ml)
			origin: aRectangle topLeft + (0@tl height);
			extent: ml width@(aRectangle height - tl height - bl height).
		(ColorFillStyle color: c)
			origin: aRectangle topLeft + tl extent;
			extent: aRectangle extent - tl extent - br extent.
		(AlphaInfiniteForm with: mr)
			origin: aRectangle topRight + (tr width negated@tr height);
			extent: mr width@(aRectangle height - tr height - br height).
		(ImageFillStyle form: bl) origin: aRectangle bottomLeft - (0@bl height).
		(AlphaInfiniteForm with: bm)
			origin: aRectangle bottomLeft - (bl width negated@bl height);
			extent: (aRectangle width - bl width - br width)@bm height.
		(ImageFillStyle form: br)
			origin: aRectangle bottomRight - br extent}
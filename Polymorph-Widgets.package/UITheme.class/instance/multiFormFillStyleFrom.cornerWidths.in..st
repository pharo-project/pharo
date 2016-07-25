fill-styles
multiFormFillStyleFrom: anArray cornerWidths: widthArray in: aRectangle
	"Answer a composite fill style from the given forms and central colour
	in the given bounds. Use the widths specified for the portion of the
	corner forms to display."
	
	|tl tm tr ml mr bl bm br c tlw trw blw brw|
	tl := anArray first. tlw:= widthArray first.
	tm := anArray second.
	tr := anArray third. trw := widthArray second.
	ml  := anArray fourth.
	c := anArray fifth.
	mr  := anArray sixth.
	bl := anArray seventh. blw := widthArray third.
	bm := anArray eighth.
	br := anArray ninth. brw := widthArray fourth.
	^CompositeFillStyle fillStyles: {
		(ImageFillStyle form: tl)
			origin: aRectangle topLeft;
			extent: tlw@tl height.
		(AlphaInfiniteForm with: tm)
			origin: aRectangle topLeft + (tlw@0);
			extent: (aRectangle width - trw - tlw)@tm height.
		(ImageFillStyle form: tr)
			origin: aRectangle topRight - (tr width@0);
			extent: trw@tr height;
			offset: (tr width - trw)@0.
		(AlphaInfiniteForm with: ml)
			origin: aRectangle topLeft + (0@tl height);
			extent: ml width@(aRectangle height - tl height - bl height).
		(ColorFillStyle color: c)
			origin: aRectangle topLeft + (tlw@tl height);
			extent: aRectangle extent - (tlw@tl height) - (brw@br height).
		(AlphaInfiniteForm with: mr)
			origin: aRectangle topRight + (trw negated@tr height);
			extent: mr width@(aRectangle height - tr height - br height).
		(ImageFillStyle form: bl)
			origin: aRectangle bottomLeft - (0@bl height);
			extent: blw@bl height.
		(AlphaInfiniteForm with: bm)
			origin: aRectangle bottomLeft - (blw negated@bl height);
			extent: (aRectangle width - blw - brw)@bm height.
		(ImageFillStyle form: br)
			origin: aRectangle bottomRight - br extent;
			extent: brw@br height;
			offset: (br width - brw)@0}
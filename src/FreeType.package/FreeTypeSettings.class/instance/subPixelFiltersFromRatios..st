accessing
subPixelFiltersFromRatios: anArray
	"Convert the ratios in anArray to a similar array containing the filter proportions as floats.
	Example:
	if <array3ofArrays5> = #((1 3 5 3 1) (1 3 5 3 1) (1 3 5 3 1))
	Then the answer is #(#(0.0769230769230769 0.2307692307692308 0.3846153846153846 0.2307692307692308 0.0769230769230769) #(0.0769230769230769 0.2307692307692308 0.3846153846153846 0.2307692307692308 0.0769230769230769) #(0.0769230769230769 0.2307692307692308 0.3846153846153846 0.2307692307692308 0.0769230769230769))"
	
	| r g b rRatios gRatios bRatios rsum gsum bsum rfilter gfilter bfilter blurR blurG blurB |
		
	r := "Color red luminance" 1.0 .
	g := "Color green luminance" 1.0 .
	b := "Color blue luminance"1.0 .
	blurR :=  anArray first.
	blurG := anArray second.
	blurB := anArray third.
	rRatios := blurR collect:[:i | r*i].
	gRatios := blurG collect:[:i | g*i].
	bRatios := blurB collect:[:i | b*i].
	"rRatios := 	{g*blurR first .	b*blurR second.	r*blurR third.	g*bl.	b*blur*blur }.							
	gRatios := 	{b*blur*blur.	r*blur.	g.		b*blur.	r*blur*blur}.
	bRatios :=	{r*blur*blur.	g*blur.	b.		r*blur.	g*blur*blur }."
	rsum := rRatios inject:0 into:[:t :i | t+i].
	gsum := gRatios inject:0 into:[:t :i | t+i].
	bsum := bRatios inject:0 into:[:t :i | t+i].
	rfilter := rRatios collect:[:e | e / rsum].
	gfilter := gRatios collect:[:e | e / gsum].
	bfilter := bRatios collect:[:e | e / bsum].
	^{rfilter. gfilter. bfilter}
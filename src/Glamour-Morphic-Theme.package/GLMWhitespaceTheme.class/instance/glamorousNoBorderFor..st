private
glamorousNoBorderFor: aMorph
	| aStyle |
	aStyle := SimpleBorder new 
			baseColor: (self glamorousLightColorFor: aMorph);
			width: 0.
	^ aStyle
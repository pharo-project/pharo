private
smallStringWidgetFor: aString color: aColor 
	^ self 
		tagWidgetFor: 
			(aString asMorph
				font: (TextStyle default fontOfSize: 6);
				color: Color lightGray;
				yourself)
		color: Color transparent
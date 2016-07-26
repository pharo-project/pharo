private
image: aForm at: aPoint sourceRect: sourceRect rule: rule alpha: sourceAlpha
	"Privately used for blending forms w/ constant alpha. Fall back to simpler case by defaul."
	^self image: aForm at: aPoint sourceRect: sourceRect rule: rule
private
image: aForm at: aPoint sourceRect: sourceRect rule: rule
	"Draw the given form. For the 'paint' combination rule use stenciling otherwise simply fill the source rectangle."
	^myCanvas image: aForm at: aPoint sourceRect: sourceRect rule: rule.
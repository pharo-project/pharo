converting
asHTMLString
	"substitute the < & > into HTML compliant elements"
	
	#($< '&lt;' $> '&gt;' $& '&amp;') pairsDo: [:k :v |
		self = k ifTrue: [^ v]].
	^ String with: self
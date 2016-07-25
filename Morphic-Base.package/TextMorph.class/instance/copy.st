copying
copy
	^ super copy text: text copy textStyle: textStyle copy 
		wrap: wrapFlag color: color
		predecessor: nil successor: nil
editing keys
blankParagraph
	| blankParagraph |
	blankParagraph  := Paragraph new.
	blankParagraph 
		compose:  '' asText 
		style:  TextStyle default copy    
		from: 1
		in:  (0@0 extent: 10@10) .
	^ blankParagraph	
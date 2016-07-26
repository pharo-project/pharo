accessing
compose 
	para
		compose: self contents asText 
		style: self textStyle
		from: 1
		in: ((frame insetBy: 4) withHeight: 9999).

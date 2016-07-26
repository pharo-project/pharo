public
post: string
	| entity client url |
	entity := ZnEntity text: string asString.
	(client := self httpClient)
		entity: entity; 
		post.
	url := client response location.
	^ url asZnUrl inContextOf: self urlProvider 
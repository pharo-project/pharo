accessing
criticsOf: aRule

	^ critiques
		select: [ :c | c rule class = aRule class ]
visiting
visit: aReference
	PostorderGuide 
		show: aReference 
		to: self 
		selecting: [ :entry | entry isSymlink not ]
as yet unclassified
assertDict: dict matchesInfo: info
	#(name id message date time author)
		do: [:sel |  (info perform: sel) ifNotNil: [:i | dict at: sel ifPresent: [:d | self assert: i = d]]].
	info ancestors 
			with: (dict at: #ancestors) 
			do: [:i :d | self assertDict: d matchesInfo: i]
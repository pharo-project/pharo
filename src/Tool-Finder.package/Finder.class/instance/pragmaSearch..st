private-pragma
pragmaSearch: aString 
	| result byCondition |
	" I choose a dictionary here because the next step is to group result by pragmas "
	result := Dictionary new.
	
	byCondition :=  self useRegEx 
		ifTrue: [[ :pragma | pragma keyword matchesRegexIgnoringCase: aString ]]
		ifFalse: [[ :pragma | pragma keyword includesSubstring: aString caseSensitive: false ]].
				
	(PragmaCollector filter: byCondition) reset; 
		do: [ :pragma |
			(result at: (pragma keyword) ifAbsentPut: OrderedCollection new)				
				add: pragma method ].
	^ result
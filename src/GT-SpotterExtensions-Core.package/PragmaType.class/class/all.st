accessing
all
	| pragmas |
	pragmas := OrderedCollection new.
	Object withAllSubclassesDo: [:each |
		Pragma withPragmasIn: each do: [:p |
			pragmas add: p]].
	^ (pragmas groupedBy: #keyword) associations collect: [ :association |
		PragmaType new
			keyword: association key; 
			pragmas: association value ] 
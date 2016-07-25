spec protocol
emphasisOptions
	^ emphasisOptions
		ifNil:
			[ 
			emphasisOptions := {(#bold -> 1).
			(#italic -> 2).
			(#underline -> 4).
			(#narrow -> 8).
			(#struck -> 16)} asDictionary ]
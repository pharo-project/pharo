private
itemMethod
	| itemClass |
	itemClass := (self model resultDictionary at: self item) first.
	^ itemClass >> self item
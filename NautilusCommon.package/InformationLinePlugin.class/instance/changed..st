announcement
changed: ann
	
	| string |
	string := self buildString.
	morph ifNotNil: [ morph contents: string ]
private
renameTagsPrefixedWith: oldName to: newName
	| oldPrefix newPrefix |
	
	oldName ifNil: [ ^ self ].
	
	self 
		classTagNamed: oldName 
		ifPresent: [ :tag | tag renameTo: newName category: newName ].	
	
	oldPrefix := oldName, '-'.
	newPrefix := newName, '-'.
	(self classTags 
		select: [ :each | each categoryName beginsWith: oldPrefix ])
		do: [ :each | 
			each 
				renameTo: each name 
				category: newPrefix, (each name) ]
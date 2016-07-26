class tags
removeClassTagsForClassNamed: aString 
	(self classTags 
		select: [ :eachTag | eachTag hasClassNamed: aString ])
		do: [ :eachTag |	
			self 
				removeClassDefinitionName: aString 
				fromClassTag: eachTag name ]
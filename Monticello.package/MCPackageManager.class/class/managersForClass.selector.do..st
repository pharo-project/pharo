system changes
managersForClass: aClass selector: aSelector do: aBlock
	^self managersForClass: aClass category: (aClass organization categoryOfElement: aSelector) do: aBlock
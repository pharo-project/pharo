private
categoriesForClass: aClass

 ^ aClass organization allMethodSelectors collect: 
			[:each |  aClass organization categoryOfElement: each].

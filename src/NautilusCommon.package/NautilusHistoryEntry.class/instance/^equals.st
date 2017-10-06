comparing
= anotherWrapper
	self species = anotherWrapper species
		ifFalse: [ ^ false ].
	^ self selectedPackage = anotherWrapper selectedPackage
		and:
			[ 
			self selectedGroup = anotherWrapper selectedGroup
				and:
					[ 
					self selectedClass = anotherWrapper selectedClass
						and:
							[ 
							self selectedCategory = anotherWrapper selectedCategory
								and:
									[ 
									self selectedSelector = anotherWrapper selectedSelector
										and:
											[ 
											self showHierarchy = anotherWrapper showHierarchy
												and:
													[ 
													self showInstance = anotherWrapper showInstance
														and:
															[ 
															self sortHierarchically = anotherWrapper sortHierarchically
																and:
																	[  
																	self showComment = anotherWrapper showComment
																		and: [ self showCategories = anotherWrapper showCategories ] ] ] ] ] ] ] ] ]
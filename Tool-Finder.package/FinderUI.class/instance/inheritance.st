private
inheritance
	
		self selectedClass ifNotNil: [ :class |.
				self selectedMethod ifNotNil: [:selector | 
					SystemNavigation new methodHierarchyBrowserForClass: class selector: selector]]
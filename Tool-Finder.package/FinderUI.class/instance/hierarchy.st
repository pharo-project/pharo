private
hierarchy
	
		self selectedClass ifNotNil: [ :class |
			self selectedMethod
				ifNil: [SystemNavigation new browseHierarchy: class]
				ifNotNil: [:selector | 
					SystemNavigation new browseHierarchy:class selector: selector]]
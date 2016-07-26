private
updateCategories
	"This method makes sure that the categories are ordered in load-order and as specified in the packages."

	| categories |
	categories := OrderedCollection new.
	self versions do: [ :version |
		version snapshot definitions do: [ :definition |
			definition isOrganizationDefinition ifTrue: [
				definition categories do: [ :category |
					(categories includes: category)
						ifFalse: [ categories addLast: category ] ] ] ] ].
	(MCOrganizationDefinition categories: categories)
		postloadOver: nil
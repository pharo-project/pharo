private
loadGroupsCategoriesInARestrictedEnvironment: env for: class
	| group |
	
	group := self model selectedGroup.
	^ self sortCategories: ((group protocolsFor: class ) intersection: ( env protocolsFor: class )) forClass: class
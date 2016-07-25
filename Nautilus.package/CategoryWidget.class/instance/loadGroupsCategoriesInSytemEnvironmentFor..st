private
loadGroupsCategoriesInSytemEnvironmentFor: class
	| group |

	group := self model selectedGroup.
	^ self sortCategories: (group protocolsFor: class ) forClass: class
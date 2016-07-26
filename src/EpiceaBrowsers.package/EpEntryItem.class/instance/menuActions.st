populating browser
menuActions

	^ (entry content accept: EpMenuActionsVisitor new), self defaultMenuActions.
private
menuActions

	^ (self eventAccept: EpMenuActionsVisitor new), self defaultMenuActions.
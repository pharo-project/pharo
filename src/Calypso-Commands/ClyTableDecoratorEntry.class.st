"
I am a table decorator declared for the table decorators of a Calypso command map: the decorator changes the cells of the items whose context is described by my anchor.
	aCommandMap tableDecorators add: ClyAbstractMethodTableDecorator
The decorator is a subclass of ClyTableDecorator. It can refuse an item with its class-side #wantsDecorateTableCellInContext:.
My priority is the class-side #priority of the decorator unless the declaration gives one. Decorators are applied in ascending priority, so the decorator of the highest priority applies its changes last.
I hold the decorator class where commands entries hold their command class (#commandClass), so the overrides of ClyCommandInteraction, such as #remove:, apply to me too.
The guide doc/Calypso/Commands.md of the Pharo repository describes the table decorators.
"
Class {
	#name : 'ClyTableDecoratorEntry',
	#superclass : 'ClyCommandEntry',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'decoration' }
ClyTableDecoratorEntry >> decorateMainTableCell: anItemCellMorph inContext: anItemContext [

	commandClass decorateMainTableCell: anItemCellMorph inContext: anItemContext
]

{ #category : 'decoration' }
ClyTableDecoratorEntry >> decorateTableCell: anItemCellMorph inContext: anItemContext [

	commandClass decorateTableCell: anItemCellMorph inContext: anItemContext
]

{ #category : 'accessing' }
ClyTableDecoratorEntry >> decoratorClass [

	^ commandClass
]

{ #category : 'activation' }
ClyTableDecoratorEntry >> isActiveInContext: anItemContext [
	"The context of the item must be described by my anchor and accepted by my decorator."

	^ (self isVisibleInContext: anItemContext) and: [ commandClass wantsDecorateTableCellInContext: anItemContext ]
]

{ #category : 'accessing' }
ClyTableDecoratorEntry >> priority [
	"The priority given by the declaration, or the one of my decorator."

	^ priority ifNil: [ commandClass priority ]
]

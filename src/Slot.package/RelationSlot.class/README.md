I'm a Slot that represents one side of a relationship. If this side of the relationship is updated I take care of keeping the other side up to date.

I'm abstract, use  the ToOne or ToMany  subclass depending on the cardinality.

Instance Variables
	inverseName:		<String>
	inverseSlot:		<RelationSlot>
	targetClass:		<Class> or <Symbol>

inverseName
	- the name  of the slot of the other side of the relationship

inverseSlot
	- the slot of the other side of the relationship

targetClass
	- the class or the name of the class this slot refers to

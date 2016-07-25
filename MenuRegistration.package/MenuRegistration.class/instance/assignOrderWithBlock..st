accessing
assignOrderWithBlock: aBlock 
	"Compute recursively MenuRegistration order"
	self itemList 
		ifNotNil: [self itemList: (aBlock value: self itemList).
				self itemList do: [:rded | rded assignOrderWithBlock: aBlock]]
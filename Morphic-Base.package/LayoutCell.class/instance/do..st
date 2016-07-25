collection
do: aBlock
	aBlock value: self.
	nextCell ifNotNil:[nextCell do: aBlock].
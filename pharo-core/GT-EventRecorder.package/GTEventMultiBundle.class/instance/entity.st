accessing
entity
	| entity |
	entity := ZnMultiPartFormDataEntity new.
	self updateEntity: entity.
	^ entity
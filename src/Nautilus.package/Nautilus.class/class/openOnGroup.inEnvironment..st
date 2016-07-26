opening
openOnGroup: aGroup inEnvironment: anEnvironment
	| instance |
	instance := self new.
	instance browsedEnvironment: anEnvironment.
	instance selectedGroup: aGroup.
	instance open.
	^ instance
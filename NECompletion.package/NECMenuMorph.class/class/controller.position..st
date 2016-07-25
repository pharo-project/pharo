instance creation
controller: aECController position: aPoint 
	| newObject |
	newObject := self new.
	newObject setController: aECController position: aPoint.
	^ newObject
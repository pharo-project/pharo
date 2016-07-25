accessing
addExtraSpace: aPoint
	extraSpace 
		ifNil:[extraSpace := aPoint]
		ifNotNil:[extraSpace := extraSpace + aPoint]
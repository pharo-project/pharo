instance creation
className: classString
classIsMeta: metaBoolean
selector: selectorString
category: catString
timeStamp: timeString
source: sourceString
	^ self instanceLike:
		(self new initializeWithClassName: classString
					classIsMeta: metaBoolean
					selector: selectorString
					category: catString
					timeStamp: timeString
					source: sourceString)
private
loadListForVersion: vrsn 

	^ (self loads == nil
			or: [self loads isEmpty])
		ifTrue: [vrsn spec defaultPackageNames]
		ifFalse: [self loads]
private
fetchRequiredFromArray: anArray
	| originalLoader displayString newLoader |
	originalLoader := self versionSpec loader.
	newLoader := originalLoader fetchingSpecLoader.
	displayString := newLoader actionLabel , self versionNumber printString , ' of ' , self spec projectLabel.
	MetacelloPlatform current
		do: [ 
			[ 
			self versionSpec loader: newLoader.
			MetacelloPlatform current
				useStackCacheDuring: [ :dict | ^ self executeLoadFromArray: anArray ]
				defaultDictionary: Dictionary new ] ensure: [ self versionSpec loader: originalLoader ] ]
		displaying: displayString
evaluating-private
writeArgumentNameIfNecessaryOf: aKeyword at: anIndex

	(self hasParameter: aKeyword) ifTrue: [ self writeArgumentNameAt: anIndex ]
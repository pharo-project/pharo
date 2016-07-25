enquiries
collectMethodsFor: aSelector into: methodDescription
	| originalSelector association |
	self subject
		collectMethodsFor: aSelector
		into: methodDescription.			
	association := self aliasNamed: aSelector ifAbsent: [nil].
	association notNil ifTrue: [
		originalSelector := association value.
		self subject
			collectMethodsFor: originalSelector
			into: methodDescription]
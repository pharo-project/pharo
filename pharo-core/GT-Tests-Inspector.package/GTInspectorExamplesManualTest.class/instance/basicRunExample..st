private
basicRunExample: anExample
	self checkAllPresentationsOf: anExample onError: [ :aPragma :anException | 
		allFailures add: anExample method.
		self printException: anException forExample: anExample withPragma: aPragma ] 
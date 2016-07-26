private
runExample: anExample
	[ self basicRunExample: anExample ]
		on: Error
		do: [ :exception | 
			allFailures add: anExample method.
			self printException: exception forExample: anExample withPragma: nil ] 
enquiries
collectMethodsFor: aSelector into: methodDescription
	"Collect instances of LocatedMethod into methodDescription
	for each method that has the selector aSelector and is not excluded
	or for which aSelector is an alias."

	self subclassResponsibility
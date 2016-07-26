filein/out
checkForSlips
	"Return a collection of method refs with possible debugging code in them."
	| slips |
	slips := OrderedCollection new.
	self changedClasses do:
		[:aClass |
		(self methodChangesAtClass: aClass name) associationsDo: 
				[:mAssoc | | method | (#(remove addedThenRemoved) includes: mAssoc value) ifFalse:
					[method := aClass compiledMethodAt: mAssoc key ifAbsent: [nil].
					method ifNotNil:
						[(self hasReportableSlip: method)
							ifTrue: [ slips add: (aClass >> mAssoc key) methodReference ]]]]].
	^ slips
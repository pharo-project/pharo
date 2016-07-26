private
writeError: anError andStack: aString for: aTestCase
	"XML tag is now open"
	anError
		ifNotNil: [
			aTestCase shouldPass ifTrue: [
				(anError isKindOf: TestFailure)
					ifTrue: [ self writeFailure: anError stack: aString ]
					ifFalse: [ self writeError: anError stack: aString ] ] ]
		ifNil: [
			aTestCase shouldPass ifFalse: [
				self writeFailure: TestFailure new stack: 'Unexpected failure' ] ]
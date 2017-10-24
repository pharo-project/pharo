tests
testObjectAllSubclasses
	"self run: #testObjectAllSubclasses"

	| n2 |
	n2 := Object allSubclasses size.
	self assert: n2 = (Object allSubclasses
			select: [:cls | cls class class == Metaclass
					or: [cls class == Metaclass]]) size
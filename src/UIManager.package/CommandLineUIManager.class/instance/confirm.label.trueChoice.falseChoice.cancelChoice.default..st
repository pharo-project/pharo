ui requests
confirm: queryString label: title trueChoice: trueLabel falseChoice: falseLabel cancelChoice: cancelLabel default: trueFalseNil
	| questions |
	questions := Dictionary new.
	trueLabel ifNotNil: [ questions at: 'y' put: trueLabel ].
	falseLabel ifNotNil: [ questions at: 'n' put: falseLabel ].
	cancelLabel ifNotNil: [ questions at: 'c' put: cancelLabel ].
	^ self choose: questions title: title, ' ', queryString

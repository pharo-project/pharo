private
colorForRule: aRule
	| total |
	^ (total := cache criticsOf: aRule)
		ifEmpty: [ self class theme textColor ]
		ifNotEmpty: [ (cache falsePositiveOf: aRule) = total
				ifTrue: [ criticModel falsePositiveColor ]
				ifFalse: [ criticModel defaultColor ] ]
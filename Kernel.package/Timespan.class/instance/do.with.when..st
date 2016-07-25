private
do: aBlock with: aFirstElement when: aConditionBlock

	| element end |
	element := aFirstElement.
	end := self end.
	[ element start <= end ] whileTrue:
	
	[(aConditionBlock value: element)
			ifTrue: [ aBlock value: element ].
		element := element next. ]
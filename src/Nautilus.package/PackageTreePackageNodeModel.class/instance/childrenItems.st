accessing
childrenItems
	| tags |

	tags := self item classTags.
	"If there is just one, do not show"
	^ (tags size = 1 and: [ tags anyOne name = self item name ])
		ifTrue: [ #() ]
		ifFalse: [ tags sorted: [ :a :b | a categoryName < b categoryName ] ]
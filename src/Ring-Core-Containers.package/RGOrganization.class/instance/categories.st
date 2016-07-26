accessing
categories
	"Retrieves categories in the system (tag-packages)"

	^self elements at: #categories ifAbsent:[ #() ]
Extension { #name : 'TextStyle' }

{ #category : '*Rubric' }
TextStyle >> privateTabsArray: anArray [
	"DefaultTab := anArray first.
	DefaultTabsArray := anArray."

	self tabsArray: anArray.
	self marginTabsArray: (self tabsArray collect: [ :t |
				 {
					 t.
					 t } ])
]

{ #category : '*Rubric' }
TextStyle >> rubTabWidth [

	^ self tabsArray first
]

Extension { #name : 'TextStyle' }

{ #category : '*Morphic-Widgets' }
TextStyle >> privateTabsArray: anArray [
	"DefaultTab := anArray first.
	DefaultTabsArray := anArray."

	self tabsArray: anArray.
	self marginTabsArray: (self tabsArray collect: [ :t |
				 {
					 t.
					 t } ])
]

{ #category : '*Morphic-Widgets' }
TextStyle >> rubTabWidth [

	^ self tabsArray first
]

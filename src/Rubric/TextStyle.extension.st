Extension { #name : 'TextStyle' }

{ #category : '*Rubric' }
TextStyle >> privateTabsArray: anArray [
	"DefaultTab := anArray first.
	DefaultTabsArray := anArray."
	tabsArray := anArray.
	marginTabsArray := tabsArray collect: [ :t | {t . t } ]
]

{ #category : '*Rubric' }
TextStyle >> rubTabWidth [
	 ^ tabsArray first
]

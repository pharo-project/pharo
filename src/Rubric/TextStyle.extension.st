Extension { #name : #TextStyle }

{ #category : #'*Rubric' }
TextStyle >> privateTabsArray: anArray [
	"DefaultTab := anArray first.
	DefaultTabsArray := anArray."
	tabsArray := anArray.
	marginTabsArray := tabsArray collect: [ :t | Array with: t with: t ]

]

{ #category : #'*Rubric' }
TextStyle >> rubTabWidth [
	 ^ tabsArray first
]

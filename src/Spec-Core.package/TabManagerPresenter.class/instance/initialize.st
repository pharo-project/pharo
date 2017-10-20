initialization
initialize
	"Initialization code for TabManagerPresenter"

	super initialize.
	tabs := OrderedCollection new asValueHolder.
	tabSelectedAction := [  ] asValueHolder.
	selectedTab := nil asValueHolder.
	self myDependents: WeakSet new.

	tabs whenChangedDo: [ :new | self widget ifNotNil: [ :w | w tabs: self getTabs value ] ].
	selectedTab whenChangedDo: [ :newTab | tabSelectedAction value cull: newTab cull: self ]
private
updatePageIndex: index
	"Change to the given page index, update the toolbar and send the announcement"

	| p oldPage |
	
	index = 0 ifTrue: [^ self].
	oldPage := self tabSelectorMorph selectedTab.
	p := self pageMorph.
	p isNil
		ifTrue: [
			self contentMorph addMorph: (self pageAt: index).]
		ifFalse: [|pageBounds|
			pageBounds := self pageMorph bounds.
			self contentMorph replaceSubmorph: p by: (self pageAt: index).
			self flag: 'Without setting here the bounds the pange moves a few pizels when refreshing the inspector'.
			self pageMorph bounds: pageBounds.
			self pageMorph layoutChanged ].
		
	((self pages at: index) toolbar notNil and: [(self pages at: index) toolbar hasSubmorphs])
		ifTrue: [
			self toolbarMorph hasSubmorphs
				ifFalse: [self toolbarMorph addMorph: (self pages at: index) toolbar ]
				ifTrue: [self toolbarMorph 
								replaceSubmorph: self toolbarMorph submorphs first 
								by: (self pages at: index) toolbar ]]
		ifFalse: [self toolbarMorph removeAllMorphs ].
		
	self headerMorph layoutChanged.
	self pageMorph layoutChanged.
	self adoptPaneColor: (self owner ifNil: [self]) paneColor.
	(self tabSelectorMorph tabs at: index) font:  self tabSelectorMorph font.
	self announcer announce: (LazyTabPageChanged new tabs: self; page: (self pages at: index); oldPage: oldPage; pageIndex: index)
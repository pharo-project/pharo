tests
testTabsWithTitleIconWithoutTitle
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a titleIcon: GLMUIThemeExtraIcons glamorousBrowse.
					a list.
					a text ].
	window := browser openOn: 42.
	self assert: self compositeTabGroup submorphs size equals: 2.
	self assert: self compositeTabGroup submorphs first class equals: GLMPanelMorph.
	self assert: self compositeTabGroup submorphs first submorphs size equals: 1.
	self assert: self compositeTabGroup submorphs first submorphs first class equals: LazyTabGroupMorph.
	self assert: self compositeTabGroup submorphs first submorphs first pages size equals: 2
icons
createIconMethods
	"self createIconMethods"
	#('smallToLeft' 'smallToRight' 'smallToLeftEnd' 'smallToRightEnd' 'smallDieze' 'smallVerticalResizing')
		do: [:n | | form |
			form := PNGReadWriter formFromFileNamed: n, '.png'.
			self class compile: (n, 'Icon', String cr, String tab, '^ ', form storeString) classified: #icons]